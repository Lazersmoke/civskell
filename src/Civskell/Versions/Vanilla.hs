{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Configurations for running Civskell as a Vanilla Minecraft server.
module Civskell.Versions.Vanilla where

import qualified Civskell.Packet.Serverbound as Server
import qualified Civskell.Block.Stone as Stone
import qualified Civskell.Packet.Clientbound as Client
import Civskell.Data.Logging
import Civskell.Data.Types
import Civskell.Tech.Network
import Civskell.Tech.Encrypt
import qualified Civskell.Entity as Entity
import Civskell.Data.Util
import Civskell.Data.Networking

import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Lens
import Data.Bytes.Put
import Data.Bytes.Serial
import qualified Data.Vector as Vector
import Data.SuchThat
import qualified Data.Set as Set
import Data.List (intercalate)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Semigroup

-- | A default packet handler that just prints an error message. For example:
--
-- @
--   'Server.handshake' $ 'unhandled' "Packet name here"
-- @
--
-- is an @'InboundPacketDescriptor' 'Server.Handshake'@.
unhandled :: T.Text -> p -> Civskell () 
unhandled t _ = loge $ "Unhandled Packet: " <> t

-- | Civskell's recommended configuration for running as a Vanilla 1.12.1 Minecraft server.
vanilla1_12_1 :: Configuration
vanilla1_12_1  = defaultConfiguration
  {packetsForState = \case
    Handshaking -> Vector.fromList 
      [ambiguate $ Server.handshake $ \(Server.Handshake protocol _addr _port newstate) -> protocolVersion <$> asks configuration >>= \prot -> if fromIntegral protocol == prot
        -- They are using the correct protocol version, so continue as they request
        -- TODO: Enum for newstate
        then case newstate of
          1 -> overContext playerData $ playerState .~ Status
          2 -> overContext playerData $ playerState .~ LoggingIn
          _ -> logp "Invalid newstate"
        -- They are using the incorrect protocol version. Disconnect packet will probably work even if they have a different version.
        else sendPacket (Client.disconnect 0x00) (Client.Disconnect (jsonyText $ "Unsupported protocol version. Please use " <> show prot))
      ,ambiguate $ Server.legacyHandshake $ \(Server.LegacyHandshake _ _ _) -> rPut . runPutS . serialize $ Client.LegacyHandshakePong
      ]
    LoggingIn -> Vector.fromList 
      -- LoginStart packets contain their username as a String
      [ambiguate $ Server.loginStart $ \(Server.LoginStart (ProtocolString name)) -> do
        -- Log that they are logging in
        logt (T.pack name) "Logging In"
        overContext playerData $ playerUsername .~ name
        -- Verify Token is fixed because why not
        -- TODO: make this a random token
        let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
        -- Server Id is blank because (((history)))
        let sId = ""
        -- Send an encryption request to the client
        sendPacket (Client.encryptionRequest 0x01) (Client.EncryptionRequest sId (LengthAnnotatedByteString encodedPublicKey) (LengthAnnotatedByteString vt))
        -- TODO
        --setPlayerState (AwaitingEncryptionResponse vt sId)
      -- Wait for them to send an Encryption Response
      ,ambiguate $ Server.encryptionResponse $ \(Server.EncryptionResponse ssFromClient vtFromClient) -> do
        -- TODO: get this from getPlayerState >>= \(AwaitingEcnryptionResponse vt sId) ->
        let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
        -- Make sure that the encryption stuff all lines up properly
        case checkVTandSS (snd globalKeypair) vtFromClient ssFromClient vt of
          -- If it doesn't, disconnect
          Left s -> sendPacket (Client.disconnect 0x00) (Client.Disconnect (jsonyText s)) >> loge (T.pack s) >> loge ("Public Key was: " <> showText (fst globalKeypair))
          -- If it does, keep going
          Right ss -> do
            -- Start encrypting our packets, now that we have the shared secret
            setupEncryption ss
            -- Make the serverId hash for auth
            let loginHash = genLoginHash "" ss encodedPublicKey
            -- Do the Auth stuff with Mojang
            name <- view playerUsername <$> fromContext playerData
            -- TODO: This uses arbitrary IO, we should make it into an effect
            serverAuthentication name loginHash >>= \case
              -- TODO: we just crash if the token is negative :3 pls fix or make it a feature
              -- If the auth is borked, its probably our fault tbh
              Nothing -> do
                loge "Failed to authenticate with Mojang"
                -- Claim guilt
                sendPacket (Client.disconnect 0x00) (Client.Disconnect $ jsonyText "Auth failed (Lazersmoke's fault, probably!)")
              Just (AuthPacket uuid nameFromAuth authProps) -> do
                -- Get the config ready because we need it a lot here
                c <- asks configuration
                pid <- _playerId <$> fromContext playerData
                overContext playerData $ playerUsername .~ nameFromAuth
                overContext playerData $ playerClientUUID .~ uuid
                -- Warning: setting this to 3 gave us a bad frame exception :S
                case compressionThreshold c of
                  -- If the config says to compress, inform the client
                  Just t -> sendPacket (Client.setCompression 0x03) (Client.SetCompression t) >> setCompression t
                  -- No compression -> don't do anything
                  Nothing -> pure ()
                -- Send a login success. We are now in play mode
                sendPacket (Client.loginSuccess 0x02) (Client.LoginSuccess (ProtocolString $ show uuid) (ProtocolString nameFromAuth))
                -- This is where the protocol specifies the state transition to be
                overContext playerData $ playerState .~ Playing
                -- 100 is the max players
                sendPacket (Client.joinGame 0x23) (Client.JoinGame pid (defaultGamemode c) (defaultDimension c) (defaultDifficulty c) (maxPlayers c) "default" False)
                -- Also sends player abilities
                overContext playerData $ playerGamemode .~ defaultGamemode c
                -- Tell them we aren't vanilla so no one gets mad at mojang for our fuck ups
                sendPacket (Client.pluginMessage 0x18) (Client.PluginMessage "MC|Brand" (runPutS $ serialize @ProtocolString "Civskell"))
                -- Difficulty to peaceful
                sendPacket (Client.serverDifficulty 0x0D) (Client.ServerDifficulty (defaultDifficulty c))
                -- World Spawn/Compass Direction, not where they will spawn initially
                sendPacket (Client.spawnPosition 0x46) (Client.SpawnPosition (spawnLocation c))
                -- Send initial world. Need a 7x7 grid or the client gets angry with us
                forM_ [0..48] $ \x -> sendPacket (Client.chunkData 0x20) =<< Client.colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
                -- Send an initial blank inventory
                sendPacket (Client.windowItems 0x14) (Client.WindowItems 0 (ProtocolList []))
                -- Give them some stone (for testing)
                setInventorySlot 4 (Slot . Just $ SlotData (Item Stone.itemStone (Stone.Stone :: Stone.Stone 'AsItem)) 32)
                ps <- mapM (lift . readTVarIO) =<< Map.elems . view worldPlayers <$> fromContext worldData
                sendPacket (Client.playerListItem 0x2E) (Client.PlayerListItem . ProtocolList $ (map (\p -> (p ^. playerClientUUID,Client.PlayerListAdd (ProtocolString $ p ^. playerUsername) (ProtocolList authProps) Survival 0 (ProtocolOptional Nothing))) ps))
      ]
    Status -> Vector.fromList 
      [ambiguate $ Server.statusRequest $ \_ -> do
        -- Get the list of connected players so we can show info about them in the server menu
        playersOnline <- mapM (lift . readTVarIO) =<< Map.elems . view worldPlayers <$> fromContext worldData
        c <- asks configuration
        let userSample = intercalate "," . map (\p -> "{\"name\":\"" <> p ^. playerUsername <> "\",\"id\":\"" <> show (p ^. playerClientUUID) <> "\"}") $ playersOnline
        let 
         resp = mconcat
          ["{\"version\":{\"name\":\""
          ,T.unpack (serverVersion c)
          ,"\",\"protocol\":"
          ,show (protocolVersion c)
          ,"},\"players\":{\"max\": 100,\"online\": "
          ,show (length playersOnline)
          ,",\"sample\":["
          ,userSample
          ,"]},\"description\":{\"text\":\""
          ,T.unpack (serverMotd c)
          ,"\"},\"favicon\":\""
          ,Server.image
          ,"\"}"
          ]
        -- Send resp to clients
        sendPacket (Client.statusResponse 0x00) (Client.StatusResponse (ProtocolString resp))
        -- TODO
        -- setPlayerState WaitingForStatusPing
      -- Send a pong with the same ping token right away
      ,ambiguate $ Server.statusPing $ \(Server.StatusPing l) -> sendPacket (Client.statusPong 0x01) (Client.StatusPong l)
      ]
    Playing -> Vector.fromList
      -- Check the tid presented against all the tid's we have stored
      [ambiguate $ Server.tpConfirm $ \(Server.TPConfirm tid) -> clearTeleport tid >>= \case
        -- If it's valid, say so
        True -> logp $ "Client confirms teleport with id: " <> showText tid
        -- If it's not, complain
        False -> loge $ "Client provided bad teleport id: " <> showText tid
      ,ambiguate $ Server.tabComplete $ unhandled "Tab Complete"
      ,ambiguate $ Server.chatMessage $ \(Server.ChatMessage (ProtocolString msg)) -> case msg of
        "/gamemode 1" -> overContext playerData $ playerGamemode .~ Creative
        "/gamemode 0" -> overContext playerData $ playerGamemode .~ Survival
        "chunks" -> forM_ [0..48] $ \x -> sendPacket (Client.chunkData 0x20) =<< Client.colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
        --"creeper" -> summonMob (Entity.Creeper Entity.defaultInsentient 0 False False)
        --"/testchest" -> do
          --let items = Map.fromList [{-(5,slot Item.Stick 3)-}]
          --i <- send (ambiguously $ newTVar items)
          --_ <- openWindowWithItems (Window.Chest i) (jsonyText "Test Chest") i
          --pure ()
        _ -> do
          broadcastPacket $ DescribedPacket (Client.chatMessage 0x0F) (Client.ChatMessage (jsonyText msg) 0)
          name <- view playerUsername <$> fromContext playerData
          logt (T.pack name) (T.pack msg)
      ,ambiguate $ Server.clientStatus $ \(Server.ClientStatus status) -> case status of
        PerformRespawn -> logp "Client wants to perform respawn"
        RequestStats -> logp "Client requests stats"
        OpenInventory -> logp "Client is opening their inventory"
      -- Teleport the client when they send this packet because reasons
      -- 0x00 means all absolute (It's a relativity flag bitfield)
      ,ambiguate $ Server.clientSettings $ \(Server.ClientSettings _loc _viewDist _chatMode _chatColors _skin _hand) -> do
        -- TODO: This *must* be transactional, and it isn't right now
        overContext playerData $ \p -> (playerTPConfirmQueue %~ Set.insert (view playerNextTid p)) . (playerNextTid %~ succ) $ p 
        tid <- view playerNextTid <$> fromContext playerData
        -- Notify the client they are being teleported
        sendPacket (Client.playerPositionAndLook 0x2F) (Client.PlayerPositionAndLook (1.0,130.0,1.0) (0.0,0.0) 0x00 tid)
      -- Check if they apologized
      ,ambiguate $ Server.confirmTransaction $ \(Server.ConfirmTransaction wid transId _acc) -> Set.member (wid,transId) . view playerFailedTransactions <$> fromContext playerData >>= \case
        True -> do
          -- Log when they aplogize
          logg $ "Client apologized for bad transaction: " <> showText (wid,transId)
          -- Remove the offending transaction
          overContext playerData $ playerFailedTransactions %~ Set.delete (wid,transId)
        False -> loge "Client apologized for non-existant transaction"
      ,ambiguate $ Server.enchantItem $ unhandled "Enchant Item"
      -- This function needs to change items in the window Id it is given
      ,ambiguate $ Server.clickWindow $ \(Server.ClickWindow wid slotNum transId mode clientProvidedSlot) -> do
        -- Log which window and slot they tried to click
        logp $ "Player clicked window " <> showText wid <> " at slot number " <> showText slotNum
        -- Get the window they are clicking
        view (playerWindows . at wid) <$> fromContext playerData >>= \case
          -- If they are clicking a window that doesn't exist, log about it
          Nothing -> loge $ "Client tried to click non-existant window with window id: " <> showText wid
          -- If they clicked a real window, make sure there are no pending failed transactions
          -- still in the queue which would prevent them from making a new transaction
          Just (SuchThat (Window desc w)) -> Set.null . view playerFailedTransactions <$> fromContext playerData >>= \case
            -- If they do have failed transactions, log about it 
            False -> loge $ "Failed, but client is still sending clicks to " <> showText (Window desc w)
            -- If they have no failed transactions, then click the window
            True -> onWindowClick desc w wid slotNum transId mode clientProvidedSlot >>= sendPacket (Client.confirmTransaction 0x11) . Client.ConfirmTransaction wid transId
      ,ambiguate $ Server.closeWindow $ \(Server.CloseWindow w) -> do
        logp $ "Player is closing a window with id: " <> showText w
        case w of
          0 -> pure ()
          wid -> overContext playerData $ playerWindows . at wid .~ Nothing
      -- BS.tail removes the length prefixing
      ,ambiguate $ Server.pluginMessage $ \case
        (Server.PluginMessage "MC|Brand" cliBrand) -> overContext playerData $ playerClientBrand .~ show (BS.tail cliBrand)
        (Server.PluginMessage chan msg) -> logp $ "Unsupported Plugin Message! " <> showText chan <> " | " <> showText msg
      ,ambiguate $ Server.useEntity $ \(Server.UseEntity targetEID _action) -> view (worldEntities . at targetEID) <$> fromContext worldData >>= \case
        _ -> error "Unimplemented: handler for UseEntity. Rework entity system" --logg $ entityName @m <> " was " <> showText action <> "(ed)"
      ,ambiguate $ Server.keepAlive $ \(Server.KeepAlive kid) -> logp $ "Player sent keep alive pong with id: " <> showText kid
      -- Explicitly ignore spammy Player packets
      ,ambiguate $ Server.player $ \(Server.Player _grounded) -> pure ()
      ,ambiguate $ Server.playerPosition $ \(Server.PlayerPosition (x,y,z) _grounded) -> overContext playerData $ playerPosition .~ (x,y,z)
      ,ambiguate $ Server.playerPositionAndLook $ \(Server.PlayerPositionAndLook (x,y,z) (yaw,pitch) _grounded) -> overContext playerData $ (playerPosition .~ (x,y,z)) . (playerViewAngle .~ (yaw,pitch))
      ,ambiguate $ Server.playerLook $ \(Server.PlayerLook (y,p) _grounded) -> overContext playerData $ playerViewAngle .~ (y,p)
      ,ambiguate $ Server.vehicleMove $ unhandled "Vehicle Move"
      ,ambiguate $ Server.steerBoat $ unhandled "Steer Boat"
      ,ambiguate $ Server.craftRecipeRequest $ unhandled "Craft Recipe Request"
      -- Only sent when flight is toggled
      ,ambiguate $ Server.playerAbilities $ \(Server.PlayerAbilities (AbilityFlags _i f _af _c) _flySpeed _fovMod) -> overContext playerData $ playerMoveMode .~ if f then Flying else Walking
      ,ambiguate $ Server.playerDigging $ \(Server.PlayerDigging action) -> case action of
        StartDig block _side -> do
          logp $ "Started digging block: " <> showText block
          -- Instant Dig
          overContext worldData $ blockInWorld block .~ ambiguate (Block air Air)
        SwapHands -> do
          -- Get the current items
          heldSlot <- view playerHoldingSlot <$> fromContext playerData
          heldItem <- getInventorySlot heldSlot
          -- 45 is the off hand slot
          offItem <- getInventorySlot 45
          -- Swap them
          ambiguously (setInventorySlot heldSlot) offItem
          ambiguously (setInventorySlot 45) heldItem
        -- No dropping from offhand
        DropItem isStack -> do
          -- Get the slot they are dropping from
          heldSlot <- view playerHoldingSlot <$> fromContext playerData
          -- Get the item in that slot
          heldItem <- getInventorySlot heldSlot
          -- Check if they are dropping literally nothing
          case heldItem of
            SuchThat (Slot Nothing) -> logp $ "Tried to drop nothing"
            SuchThat (Slot (Just heldData)) -> do
              -- Drop the entire stack, or at most one item
              let (dropped,newHeld) = takeFromSlot (if isStack then 64 else 1) $ heldData
              -- Log the SlotData they just dropped
              logp $ "Dropping: " <> showText dropped
              -- Set their inventory to match the new dropped version
              setInventorySlot heldSlot newHeld
              -- Get their location so we know where to drop the item at
              plaLoc <- view playerPosition <$> fromContext playerData
              -- A standard base entity to graft the item onto, centered at the player
              let zeroedBaseEntity = Entity.BaseEntity (EntityLocation plaLoc (0,0)) (EntityVelocity (0,0,0)) 0x00 300 "" False False False
              -- Summon the new item entity with the dropped SlotData
              summonObject . some $ Entity.ItemEntity zeroedBaseEntity dropped
        _ -> loge $ "Unhandled Player Dig Action!"
      -- We know the eid because its us
      ,ambiguate $ Server.entityAction $ \(Server.EntityAction _eid action) -> case action of
        Sneak True -> overContext playerData $ playerMoveMode .~ Sneaking
        Sneak False -> overContext playerData $ playerMoveMode .~ Walking
        Sprint True -> overContext playerData $ playerMoveMode .~ Sprinting
        Sprint False -> overContext playerData $ playerMoveMode .~ Walking
        HorseJumpStart _ -> logp "Jumping with horse"
        HorseJumpStop -> logp "Stopped Jumping with horse"
        LeaveBed -> pure ()
        HorseInventory -> pure () -- Open window here?
        ElytraFly -> pure () -- ?
      ,ambiguate $ Server.steerVehicle $ unhandled "Steer Vehicle"
      ,ambiguate $ Server.craftingBookData $ unhandled "Crafting Book Data"
      ,ambiguate $ Server.resourcePackStatus $ unhandled "Resource Pack Status"
      ,ambiguate $ Server.advancementTab $ unhandled "Advancement Tab"
      -- Update the slot they are holding in the player data
      ,ambiguate $ Server.heldItemChange $ \(Server.HeldItemChange slotNum) -> overContext playerData $ playerHoldingSlot .~ slotNum
      -- Clients handle all the dirty details such that this is just a "set slot" packet
      ,ambiguate $ Server.creativeInventoryAction $ \(Server.CreativeInventoryAction slotNum slotDat) -> parseItemFromContext slotDat >>= \case
        Nothing -> loge $ "Unable to parse wire slot from creative inventory action: " <> showText slotDat <> ". Aborting action."
        Just s -> do
          logp $ "Player creatively set slot " <> showText slotNum <> " to {" <> ambiguously showText s <> "}"
          -- TODO: This will echo back a SetSlot packet to the client, while they should already know about the change because they initiated it. 
          -- Add another way to access the effect inventory?
          -- Maybe that is ok? idk requires further testing to be sure
          ambiguously (setInventorySlot slotNum) s
      ,ambiguate $ Server.updateSign $ unhandled "Update Sign"
      -- Don't do anything about the spammy animation packets
      ,ambiguate $ Server.animation $ \(Server.Animation _anim) -> pure ()
      ,ambiguate $ Server.spectate $ unhandled "Spectate"
      ,ambiguate $ Server.playerBlockPlacement $ \(Server.PlayerBlockPlacement block side hand cursorCoord) -> do
        -- Get the block they placed on
        SuchThat (Block desc b) <- view (blockInWorld block) <$> fromContext worldData
        case onClick desc of
          Just cb -> cb b block side hand cursorCoord
          Nothing -> do
            -- Find out what item they are trying to place
            heldSlot <- if hand == MainHand then view playerHoldingSlot <$> fromContext playerData else pure 45
            -- The actual slot is the `heldSlot`th slot in their hotbar, so
            -- add 36 to bypass the rest of the inventory
            let actualSlot = heldSlot + 36
            theItem <- getInventorySlot actualSlot
            case theItem of
              -- If they right click with an empty hand, this will happen
              SuchThat (Slot Nothing) -> logp "No item to use"
              -- If they right click with a real item, check if that item has a usage continuation or consumption action
              SuchThat (Slot (Just theSlotData@(SlotData (Item idesc i) _cnt))) -> case onItemUse idesc of
                -- If they right click with an item that doesn't get used, this will happen
                Nothing -> logp "No onItemUse for item"
                -- If they right click with an item that has an action
                Just (dItem,oiu)-> do
                  -- Then apply that item's "consumption" action to the inventory slot
                  overContext playerData $ playerInventory . at actualSlot .~ Just (ambiguate $ dItem theSlotData)
                  -- And run that item's continuation
                  oiu (Item idesc i) block side hand cursorCoord
      ,ambiguate $ Server.useItem $ \(Server.UseItem hand) -> do
        -- Decide which slot they are using, and find the item in that hand
        held <- getInventorySlot =<< (case hand of {MainHand -> view playerHoldingSlot <$> fromContext playerData; OffHand -> pure 45})
        logp $ "Used: " <> ambiguously showText held
      ]
  ,serverVersion = "Vanilla 1.12.1"
  ,protocolVersion = 338
  }

{-
vanilla1_12_1 :: Configuration
vanilla1_12_1 = vanilla1_12
  {packetsForState = \case
    Playing -> moveVec 0x01 0x12 (packetsForState vanilla1_12 Playing)
    x -> packetsForState vanilla1_12 x
  ,serverVersion = "Vanilla 1.12.1"
  ,protocolVersion = 338
  }
-}
