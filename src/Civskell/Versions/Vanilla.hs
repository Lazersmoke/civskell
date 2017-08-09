{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Civskell.Versions.Vanilla where

import qualified Civskell.Packet.Serverbound as Server
import qualified Civskell.Block.Stone as Stone
import qualified Civskell.Packet.Clientbound as Client
import Civskell.Data.Logging
import Civskell.Data.World
import Civskell.Data.Types
import Civskell.Tech.Network
import Civskell.Tech.Encrypt
import qualified Civskell.Entity as Entity
import qualified Civskell.Window as Window
import Civskell.Data.Player

import Control.Eff
import Control.Eff.Reader (ask)
import Data.Bytes.Put
import Data.Bytes.Serial
import qualified Data.Vector as Vector
import Data.SuchThat
import Control.Monad
import qualified Data.Set as Set
import Data.List (intercalate)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Functor.Identity
import Data.Semigroup

unhandled :: CanHandlePackets r => T.Text -> p -> Eff r () 
unhandled t _ = loge $ "Unhandled Packet: " <> t

vanilla1_12_1 :: Configuration
vanilla1_12_1  = defaultConfiguration
  {packetsForState = \case
    Handshaking -> Vector.fromList 
      [ambiguate $ Server.handshake $ \(Server.Handshake protocol _addr _port newstate) -> protocolVersion <$> ask >>= \prot -> if fromIntegral protocol == prot
        -- They are using the correct protocol version, so continue as they request
        -- TODO: Enum for newstate
        then case newstate of
          1 -> setPlayerState Status
          2 -> setPlayerState LoggingIn
          _ -> logp "Invalid newstate"
        -- They are using the incorrect protocol version. Disconnect packet will probably work even if they have a different version.
        else sendPacket (Client.disconnect 0x00) (Client.Disconnect (jsonyText $ "Unsupported protocol version. Please use " <> show prot))
      ,ambiguate $ Server.legacyHandshake $ \(Server.LegacyHandshake _ _ _) -> iSolemnlySwearIHaveNoIdeaWhatImDoing . runPutS . serialize $ Client.LegacyHandshakePong
      ]
    LoggingIn -> Vector.fromList 
      -- LoginStart packets contain their username as a String
      [ambiguate $ Server.loginStart $ \(Server.LoginStart (ProtocolString name)) -> do
        -- Log that they are logging in
        logt (T.pack name) "Logging In"
        setUsername name
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
            beginEncrypting ss
            -- Make the serverId hash for auth
            let loginHash = genLoginHash "" ss encodedPublicKey
            -- Do the Auth stuff with Mojang
            name <- clientUsername <$> getPlayer
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
                c <- ask
                pid <- registerPlayer
                setUsername nameFromAuth
                setUUID uuid
                -- Warning: setting this to 3 gave us a bad frame exception :S
                case compressionThreshold c of
                  -- If the config says to compress, inform the client
                  Just t -> sendPacket (Client.setCompression 0x03) (Client.SetCompression t) >> beginCompression t
                  -- No compression -> don't do anything
                  Nothing -> pure ()
                -- Send a login success. We are now in play mode
                sendPacket (Client.loginSuccess 0x02) (Client.LoginSuccess (ProtocolString $ show uuid) (ProtocolString nameFromAuth))
                -- This is where the protocol specifies the state transition to be
                setPlayerState Playing
                -- 100 is the max players
                sendPacket (Client.joinGame 0x23) (Client.JoinGame pid (defaultGamemode c) (defaultDimension c) (defaultDifficulty c) (maxPlayers c) "default" False)
                -- Also sends player abilities
                setGamemode (defaultGamemode c)
                -- Tell them we aren't vanilla so no one gets mad at mojang for our fuck ups
                sendPacket (Client.pluginMessage 0x18) (Client.PluginMessage "MC|Brand" (runPutS $ serialize @ProtocolString "Civskell"))
                -- Difficulty to peaceful
                sendPacket (Client.serverDifficulty 0x0D) (Client.ServerDifficulty (defaultDifficulty c))
                -- World Spawn/Compass Direction, not where they will spawn initially
                sendPacket (Client.spawnPosition 0x46) (Client.SpawnPosition (spawnLocation c))
                -- Send initial world. Need a 7x7 grid or the client gets angry with us
                forM_ [0..48] $ \x -> sendPacket (Client.chunkData 0x20) =<< colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
                -- Send an initial blank inventory
                sendPacket (Client.windowItems 0x14) (Client.WindowItems 0 (ProtocolList []))
                -- Give them some stone (for testing)
                setInventorySlot 4 (Slot . Just $ SlotData (some (Stone.Stone :: Stone.Stone 'AsItem)) 32)
                ps <- allPlayers
                sendPacket (Client.playerListItem 0x2E) (Client.PlayerListItem . ProtocolList $ (map (\p -> (clientUUID p,PlayerListAdd (ProtocolString $ clientUsername p) (ProtocolList authProps) Survival 0 (ProtocolOptional Nothing))) ps))
      ]
    Status -> Vector.fromList 
      [ambiguate $ Server.statusRequest $ \_ -> do
        -- Get the list of connected players so we can show info about them in the server menu
        playersOnline <- allPlayers
        motd <- serverMotd <$> ask
        proto <- protocolVersion <$> ask
        vers <- serverVersion <$> ask
        let userSample = intercalate "," . map (\p -> "{\"name\":\"" <> clientUsername p <> "\",\"id\":\"" <> show (clientUUID p) <> "\"}") $ playersOnline
        let resp = "{\"version\":{\"name\":\"" <> T.unpack vers <> "\",\"protocol\":" <> show proto <> "},\"players\":{\"max\": 100,\"online\": " <> show (length playersOnline) <> ",\"sample\":[" <> userSample <> "]},\"description\":{\"text\":\"" <> T.unpack motd <> "\"},\"favicon\":\"" <> {-image-} "" <> "\"}"
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
        "/gamemode 1" -> setGamemode Creative
        "/gamemode 0" -> setGamemode Survival
        "chunks" -> forM_ [0..48] $ \x -> sendPacket (Client.chunkData 0x20) =<< colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
        "creeper" -> summonMob (Entity.Creeper Entity.defaultInsentient 0 False False)
        "/testchest" -> do
          let items = Map.fromList [{-(5,slot Item.Stick 3)-}]
          i <- send (WorldSTM $ newTVar items)
          _ <- openWindowWithItems (Window.Chest i) (jsonyText "Test Chest") i
          pure ()
        _ -> do
          broadcastPacket (Client.chatMessage 0x0F) (Client.ChatMessage (jsonyText msg) 0)
          name <- clientUsername <$> getPlayer
          logt (T.pack name) (T.pack msg)
      ,ambiguate $ Server.clientStatus $ \(Server.ClientStatus status) -> case status of
        PerformRespawn -> logp "Client wants to perform respawn"
        RequestStats -> logp "Client requests stats"
        OpenInventory -> logp "Client is opening their inventory"
      -- Teleport the client when they send this packet because reasons
      -- 0x00 means all absolute (It's a relativity flag bitfield)
      ,ambiguate $ Server.clientSettings $ \(Server.ClientSettings _loc _viewDist _chatMode _chatColors _skin _hand) -> pendTeleport (1.0,130.0,1.0) (0.0,0.0) 0x00
      ,ambiguate $ Server.confirmTransaction $ \(Server.ConfirmTransaction wid transId _acc) -> do
        apologized <- Set.member (wid,transId) . failedTransactions <$> getPlayer
        if apologized
          then do
            logg "Client apologized for bad transaction"
            overPlayer $ \p -> p {failedTransactions = Set.delete (wid,transId) $ failedTransactions p}
          else loge "Client apologized for non-existant transaction"
      ,ambiguate $ Server.enchantItem $ unhandled "Enchant Item"
      -- This function needs to change items in the window Id it is given
      ,ambiguate $ Server.clickWindow $ \(Server.ClickWindow wid slotNum transId mode _clientProvidedSlot) -> do
        logp $ "Player clicked window " <> showText wid <> " at slot number " <> showText slotNum
        failing <- not . Set.null . failedTransactions <$> getPlayer
        (SuchThat (Identity (w :: wt))) <- flip (Map.!) wid . windows <$> getPlayer
        if failing 
          then loge "Failed, but client is still sending clicks" 
          else onWindowClick @wt w wid slotNum transId mode >>= sendPacket (Client.confirmTransaction 0x11) . Client.ConfirmTransaction wid transId
      ,ambiguate $ Server.closeWindow $ \(Server.CloseWindow w) -> do
        logp $ "Player is closing a window with id: " <> showText w
        case w of
          0 -> pure ()
          wid -> overPlayer $ \p -> p {windows = Map.delete wid (windows p)}
      -- BS.tail removes the length prefixing
      ,ambiguate $ Server.pluginMessage $ \case
        (Server.PluginMessage "MC|Brand" cliBrand) -> setBrand $ show (BS.tail cliBrand)
        _ -> logp $ "Unsupported Plugin Message!"
      ,ambiguate $ Server.useEntity $ \(Server.UseEntity targetEID action) -> do
        (SuchThat (Identity (_ :: m))) <- getEntity targetEID
        logg $ entityName @m <> " was " <> showText action <> "(ed)"
      ,ambiguate $ Server.keepAlive $ \(Server.KeepAlive kid) -> logp $ "Player sent keep alive pong with id: " <> showText kid
      -- Explicitly ignore spammy Player packets
      ,ambiguate $ Server.player $ \(Server.Player _grounded) -> pure ()
      ,ambiguate $ Server.playerPosition $ \(Server.PlayerPosition (x,y,z) _grounded) -> setPlayerPos (x,y,z)
      ,ambiguate $ Server.playerPositionAndLook $ \(Server.PlayerPositionAndLook (x,y,z) (yaw,pitch) _grounded) -> do
        setPlayerPos (x,y,z)
        setPlayerViewAngle (yaw,pitch)
      ,ambiguate $ Server.playerLook $ \(Server.PlayerLook (y,p) _grounded) -> setPlayerViewAngle (y,p)
      ,ambiguate $ Server.vehicleMove $ unhandled "Vehicle Move"
      ,ambiguate $ Server.steerBoat $ unhandled "Steer Boat"
      ,ambiguate $ Server.craftRecipeRequest $ unhandled "Craft Recipe Request"
      -- Only sent when flight is toggled
      ,ambiguate $ Server.playerAbilities $ \(Server.PlayerAbilities (AbilityFlags _i f _af _c) _flySpeed _fovMod) -> if f then setMoveMode Flying else setMoveMode Walking
      ,ambiguate $ Server.playerDigging $ \(Server.PlayerDigging action) -> case action of
        StartDig block _side -> do
          logp $ "Started digging block: " <> showText block
          -- Instant Dig
          removeBlock block
        SwapHands -> do
          -- Get the current items
          heldSlot <- holdingSlot <$> getPlayer
          heldItem <- getInventorySlot heldSlot
          -- 45 is the off hand slot
          offItem <- getInventorySlot 45
          -- Swap them
          setInventorySlot heldSlot offItem
          setInventorySlot 45 heldItem
        -- No dropping from offhand
        DropItem isStack -> do
          heldSlot <- holdingSlot <$> getPlayer
          heldItem <- getInventorySlot heldSlot
          -- Drop the entire stack, or at most one item
          let (dropped,newHeld) = splitStack (if isStack then 64 else 1) heldItem
          logp $ "Dropping: " <> showText dropped
          setInventorySlot heldSlot newHeld
          -- This needs to be qualified because the `playerPosition` packet
          -- descriptor is imported qualified everywhere else, but is in our
          -- local namespace in this module
          plaLoc <- Civskell.Data.Types.playerPosition <$> getPlayer
          summonObject (Entity.Item (Entity.BaseEntity (EntityLocation plaLoc (0,0)) (EntityVelocity (0,0,0)) 0x00 300 "" False False False) ((\(Slot (Just x)) -> x) dropped))
        _ -> loge $ "Unhandled Player Dig Action!"
      -- We know the eid because its us
      ,ambiguate $ Server.entityAction $ \(Server.EntityAction _eid action) -> case action of
        Sneak True -> setMoveMode Sneaking
        Sneak False -> setMoveMode Walking
        Sprint True -> setMoveMode Sprinting
        Sprint False -> setMoveMode Walking
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
      ,ambiguate $ Server.heldItemChange $ \(Server.HeldItemChange slotNum) -> setHolding slotNum
      -- Clients handle all the dirty details such that this is just a "set slot" packet
      ,ambiguate $ Server.creativeInventoryAction $ \(Server.CreativeInventoryAction slotNum slotDat) -> do
        logp $ "Player creatively set slot " <> showText slotNum <> " to {" <> showText slotDat <> "}"
        -- TODO: This will echo back a SetSlot packet to the client, while they should already know about the change because they initiated it. 
        -- Add another way to access the effect inventory?
        -- Maybe that is ok? idk requires further testing to be sure
        setInventorySlot slotNum slotDat
      ,ambiguate $ Server.updateSign $ unhandled "Update Sign"
      -- Don't do anything about the spammy animation packets
      ,ambiguate $ Server.animation $ \(Server.Animation _anim) -> pure ()
      ,ambiguate $ Server.spectate $ unhandled "Spectate"
      ,ambiguate $ Server.playerBlockPlacement $ \(Server.PlayerBlockPlacement block side hand cursorCoord) -> do
        sb <- getBlock block
        let oc = ambiguously (\(Identity (b :: bt)) -> ($b) <$> onClick @bt) sb
        case oc of
          Just cb -> cb block side hand cursorCoord
          Nothing -> do
            -- Find out what item they are trying to place
            heldSlot <- if hand == MainHand then holdingSlot <$> getPlayer else pure 45
            msl <- getInventorySlot (heldSlot + 36)
            case msl of
              Slot Nothing -> logp "No item to use"
              Slot (Just (SlotData (SuchThat (Identity (i :: it))) _cnt)) -> case onItemUse @it of
                -- If they right click on a block with an empty hand, this will happen
                Nothing -> logp "No onItemUse for item"
                Just oiu -> oiu i block side hand cursorCoord
                -- Remove item from inventory
                --let newSlot = if icount == 1 then EmptySlot else (Slot i (icount - 1))
                --setInventorySlot heldSlot newSlot
                -- TODO: map item damage to block damage somehow
                --setBlock (Tile.Chest North []) (blockOnSide block side)
      ,ambiguate $ Server.useItem $ \(Server.UseItem hand) -> do
        -- Decide which slot they are using, and find the item in that hand
        held <- getInventorySlot =<< (case hand of {MainHand -> holdingSlot <$> getPlayer; OffHand -> pure 45})
        logp $ "Used: " <> showText held
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
