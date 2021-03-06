{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Utility functions for writing protocol configs
module Civskell.Versions.Vanilla where

import qualified Civskell.Packet.Serverbound as Server
import qualified Civskell.Block.Stone as Stone
import qualified Civskell.Packet.Clientbound as Client
import Civskell.Data.Logging
import Civskell.Data.Types
import Civskell.Tech.Network
import Civskell.Tech.Encrypt
import qualified Civskell.Entity as Entity
import Civskell.Data.Networking

import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Lens
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.SuchThat
import qualified Data.Set as Set
import Data.List (intercalate)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Semigroup
import Data.Void
import Control.Concurrent

-- | @'reapeatedly' s0 a timeout@ does the action @a@ every @timeout@ microseconds, passing the state back into it, starting with @s0@.
repeatedly :: s -> (s -> Civskell s) -> Int -> Civskell Void
repeatedly s0 a timeout = loop s0
  where
    loop s = do
      lift (threadDelay timeout)
      a s >>= loop

-- | Sends the specified keep-alive packet if there are any players online, and returns a state one higher than was passed.
-- This is intended to be used with @'repeatedly'@.
keepAliveAction :: Enum s => (s -> DescribedPacket PacketSerializer p) -> s -> Civskell s
keepAliveAction mkPkt s = do
  Map.null . view worldPlayers <$> fromContext worldData >>= \case
    -- Don't do anything if no one is listening
    True -> pure ()
    -- If there are players, then broadcast keep alives
    False -> do
      -- Send everyone a keep alive packet
      broadcastPacket $ mkPkt s
      logLevel VerboseLog "Broadcasting Keepalives"
  -- Return the updated state for next time
  pure (succ s)

-- | Set the player's @'Gamemode'@, and also notify the client that their @'Gamemode'@ has changed.
setGamemode :: Gamemode -> Civskell ()
setGamemode gm = do
  overContext playerData $ playerGamemode .~ gm
  sendPacket (Client.changeGameState 0x1E) (Client.ChangeGameState $ ChangeGamemode gm)
  case gm of
    Survival -> sendPacket (Client.playerAbilities 0x2C) (Client.PlayerAbilities (AbilityFlags False False False False) 0 1)
    Creative -> sendPacket (Client.playerAbilities 0x2C) (Client.PlayerAbilities (AbilityFlags True False True True) 0 1)

-- | Set the given @'Block'@ at the given @'BlockCoord'@, and also notify the client that the block has been set.
setBlock :: BlockCoord -> ForAny Block -> Civskell ()
setBlock bc b = do
  overContext worldData $ blockInWorld bc .~ b
  sendPacket (Client.blockChange 0x0B) (Client.BlockChange bc (ambiguously toWireBlock b))

-- | Disconnect if they have the wrong version, otherwise set the player state and continue.
handleHandshake :: Server.Handshake -> Civskell ()
handleHandshake (Server.Handshake protocol _addr _port newstate) = case newstate of
  -- TODO: Enum for newstate
  1 -> overContext playerData $ playerState .~ Status
  -- If the want to log in, check their protocol version
  2 -> protocolVersion <$> asks configuration >>= \prot -> if fromIntegral protocol == prot
  -- They are using the correct protocol version, so continue as they request
    then overContext playerData $ playerState .~ LoggingIn
    -- They are using the incorrect protocol version. Disconnect packet will probably work even if they have a different version.
    else sendPacket (Client.disconnect 0x00) (Client.Disconnect (jsonyText $ "Unsupported protocol version. Please use " <> show prot))
  _ -> logp "Invalid newstate"

-- | Send back the canned legacy handshake response
handleLegacyHandshake :: Server.LegacyHandshake -> Civskell ()
handleLegacyHandshake _ = rPut . runPutS . serialize $ Client.LegacyHandshakePong

-- | Set the player's name to the one provided and begin the
-- encryption handshake process
handleLoginStart :: Server.LoginStart -> Civskell ()
handleLoginStart (Server.LoginStart (ProtocolString name)) = do
  -- LoginStart packets contain their username as a String
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

-- | Complete the server's side of the encryption handshake, upto sending the client the world.
-- They will be spawned after this handler finishes.
handleEncryptionResponse :: Server.EncryptionResponse -> Civskell ()
handleEncryptionResponse (Server.EncryptionResponse ssFromClient vtFromClient) = do
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
          -- Add this player to the world with a fresh Player Id
          wor <- asks worldData
          pTvar <- asks playerData
          pid <- lift . atomically $ do
            pid <- view worldNextEID <$> readTVar wor
            modifyTVar wor $ (worldPlayers . at pid) .~ Just pTvar
            pure pid
          overContext playerData $ \p -> p {_playerId = pid}
          -- 100 is the max players
          sendPacket (Client.joinGame 0x23) (Client.JoinGame pid (defaultGamemode c) (defaultDimension c) (defaultDifficulty c) (maxPlayers c) "default" False)
          -- Also sends player abilities
          setGamemode $ defaultGamemode c
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
          setInventorySlot 4 (Slot . Just $ SlotData (Item Stone.itemStone (Stone.Stone :: Stone.Stone)) 32)
          ps <- mapM (lift . readTVarIO) =<< Map.elems . view worldPlayers <$> fromContext worldData
          sendPacket (Client.playerListItem 0x2E) (Client.PlayerListItem . ProtocolList $ (map (\p -> (p ^. playerClientUUID,Client.PlayerListAdd (ProtocolString $ p ^. playerUsername) (ProtocolList authProps) Survival 0 (ProtocolOptional Nothing))) ps))

-- | Responds to a server list ping with the server's icon, motd, player list, etc.
handleStatusRequest :: Server.StatusRequest -> Civskell ()
handleStatusRequest _ = do
  -- Get the list of connected players so we can show info about them in the server menu
  playersOnline <- mapM (lift . readTVarIO) =<< Map.elems . view worldPlayers <$> fromContext worldData
  c <- asks configuration
  let 
    userSample = intercalate "," . map (\p -> "{\"name\":\"" <> p ^. playerUsername <> "\",\"id\":\"" <> show (p ^. playerClientUUID) <> "\"}") $ playersOnline
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

-- | Send a pong with the same ping token right away
handleStatusPing :: Server.StatusPing -> Civskell ()
handleStatusPing (Server.StatusPing l) = sendPacket (Client.statusPong 0x01) (Client.StatusPong l)

-- | Remove the teleport from the pending list and log it
handleTPConfirm :: Server.TPConfirm -> Civskell ()
handleTPConfirm (Server.TPConfirm tid) = clearTeleport tid >>= \case
  -- Check the tid presented against all the tid's we have stored
  -- If it's valid, say so
  True -> logp $ "Client confirms teleport with id: " <> showText tid
  -- If it's not, complain
  False -> loge $ "Client provided bad teleport id: " <> showText tid

-- | Broadcast the chat message to everyone, or simple commands
handleChatMessage :: Server.ChatMessage -> Civskell ()
handleChatMessage (Server.ChatMessage (ProtocolString msg)) = case msg of
  "/gamemode 1" -> setGamemode Creative
  "/gamemode 0" -> setGamemode Survival
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

-- | Just log the event.
handleClientStatus :: Server.ClientStatus -> Civskell ()
handleClientStatus (Server.ClientStatus status) = case status of
  PerformRespawn -> logp "Client wants to perform respawn"
  RequestStats -> logp "Client requests stats"
  OpenInventory -> logp "Client is opening their inventory"

-- | Teleport the client to (1,130,1) because reasons.
-- Namely, this is sent when they are done loading the world.
handleClientSettings :: Server.ClientSettings -> Civskell ()
handleClientSettings (Server.ClientSettings _loc _viewDist _chatMode _chatColors _skin _hand) = do
  -- Teleport the client when they send this packet because reasons
  -- 0x00 means all absolute (It's a relativity flag bitfield)
  pt <- asks playerData
  tid <- lift . atomically $ do
    nextTid <- view playerNextTid <$> readTVar pt
    modifyTVar pt $ (playerTPConfirmQueue %~ Set.insert nextTid) . (playerNextTid %~ succ)
    pure nextTid
  -- Notify the client they are being teleported
  sendPacket (Client.playerPositionAndLook 0x2F) (Client.PlayerPositionAndLook (1.0,130.0,1.0) (0.0,0.0) 0x00 tid)

-- | Remove apologized transactions from the list
handleConfirmTransaction :: Server.ConfirmTransaction -> Civskell ()
handleConfirmTransaction (Server.ConfirmTransaction wid transId _acc) = do
  plas <- asks playerData
  isExisting <- lift . atomically $ do
    isExisting <- Set.member (wid,transId) . view playerFailedTransactions <$> readTVar plas
    -- Remove the offending transaction
    when isExisting $ modifyTVar plas $ playerFailedTransactions %~ Set.delete (wid,transId)
    pure isExisting
  if isExisting
    -- Log when they aplogize
    then logg $ "Client apologized for bad transaction: " <> showText (wid,transId)
    else loge "Client apologized for non-existant transaction"

-- | Handle the click using the window's method and send a @'Client.ConfirmTransaction'@
handleClickWindow :: Server.ClickWindow -> Civskell ()
handleClickWindow (Server.ClickWindow wid slotNum transId mode clientProvidedSlot) = do
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
      False -> loge $ "Window click failed, but client is still sending clicks to " <> showText (Window desc w)
      -- If they have no failed transactions, then click the window
      True -> onWindowClick desc w wid slotNum transId mode clientProvidedSlot >>= sendPacket (Client.confirmTransaction 0x11) . Client.ConfirmTransaction wid transId

-- | Close the indicated window by removing it from the list
handleCloseWindow :: Server.CloseWindow -> Civskell ()
handleCloseWindow (Server.CloseWindow w) = do
  logp $ "Player is closing a window with id: " <> showText w
  case w of
    0 -> pure ()
    wid -> overContext playerData $ playerWindows . at wid .~ Nothing

-- | Respond to known plugin messages, log otherwise.
-- Known plugin channels:
--
--   * @MC|Brand@ - Set the client brand
handlePluginMessage :: Server.PluginMessage -> Civskell ()
handlePluginMessage = \case
  -- BS.tail removes the length prefixing
  (Server.PluginMessage "MC|Brand" cliBrand) -> overContext playerData $ playerClientBrand .~ show (BS.tail cliBrand)
  (Server.PluginMessage chan msg) -> lognyi $ "Unsupported Plugin Message: " <> showText chan <> " | " <> showText msg

-- | Unimplemented handler stub
handleUseEntity :: Server.UseEntity -> Civskell ()
handleUseEntity (Server.UseEntity targetEID _action) = view (worldEntities . at targetEID) <$> fromContext worldData >>= \case
  _ -> error "Unimplemented: handler for UseEntity. Rework entity system" --logg $ entityName @m <> " was " <> showText action <> "(ed)"

-- | Log that the player ponged the keep alive
handleKeepAlive :: (Serial a,Show a) => Server.KeepAlive a -> Civskell ()
handleKeepAlive (Server.KeepAlive kid) = logp $ "Player sent keep alive pong with id: " <> showText kid

-- | Explicitly ignore spammy Player packets
handlePlayer :: Server.Player -> Civskell ()
handlePlayer (Server.Player _grounded) = pure ()

-- | Update the player's position to match the one sent in the packet
handlePlayerPosition :: Server.PlayerPosition -> Civskell ()
handlePlayerPosition (Server.PlayerPosition (x,y,z) _grounded) = overContext playerData $ playerPosition .~ (x,y,z)

-- | Update the player's position and looking direction to match the one sent in the packet
handlePlayerPositionAndLook :: Server.PlayerPositionAndLook -> Civskell ()
handlePlayerPositionAndLook (Server.PlayerPositionAndLook (x,y,z) (yaw,pitch) _grounded) = overContext playerData $ (playerPosition .~ (x,y,z)) . (playerViewAngle .~ (yaw,pitch))

-- | Update the player's looking direction to match the one sent in the packet
handlePlayerLook :: Server.PlayerLook -> Civskell ()
handlePlayerLook (Server.PlayerLook (y,p) _grounded) = overContext playerData $ playerViewAngle .~ (y,p)

{-
handleVehicleMove :: Server.VehicleMove -> Civskell ()
handleVehicleMove

handleSteerBoat :: Server.SteerBoat -> Civskell ()
handleSteerBoat
-}

-- | Update the player's abilities to match those sent in the packet.
-- TODO: investigate why this packet even exists.
handlePlayerAbilities :: Server.PlayerAbilities -> Civskell ()
handlePlayerAbilities (Server.PlayerAbilities (AbilityFlags _i f _af _c) _flySpeed _fovMod) = overContext playerData $ playerMoveMode .~ if f then Flying else Walking

-- | Depending on the action:
--
-- * Instantly break the block for @'StartDig'@.
-- * Swap the items in their hands for @'SwapHands'@.
-- * Drop the item they are holding for @'DropItem'@.
handlePlayerDigging :: Server.PlayerDigging -> Civskell ()
handlePlayerDigging (Server.PlayerDigging action) = case action of
  StartDig block _side -> do
    logp $ "Started digging block: " <> showText block
    -- Instant Dig
    setBlock block $ ambiguate (Block air Air)
  SwapHands -> do
    -- Get the current items
    heldSlot <- slotInHand MainHand
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
    getInventorySlot heldSlot >>= \case
      -- Check if they are dropping literally nothing
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
  _ -> lognyi $ "Unhandled Player Dig Action: " <> showText action

-- | Set the move mode or log about unsupported entity action
handleEntityAction :: Server.EntityAction -> Civskell ()
handleEntityAction (Server.EntityAction _eid action) = case action of
  -- We know the eid because its us
  Sneak True -> overContext playerData $ playerMoveMode .~ Sneaking
  Sneak False -> overContext playerData $ playerMoveMode .~ Walking
  Sprint True -> overContext playerData $ playerMoveMode .~ Sprinting
  Sprint False -> overContext playerData $ playerMoveMode .~ Walking
  _ -> lognyi $ "Unimplemented Entity Action: " <> showText action

{-
handleSteerVehicle :: Server.SteerVehicle -> Civskell ()
handleSteerVehicle


handleCraftingBookData :: Server.CraftingBookData -> Civskell ()
handleCraftingBookData


handleResourcePackStatus :: Server.ResourcePackStatus -> Civskell ()
handleResourcePackStatus


handleAdvancementTab :: Server.AdvancementTab -> Civskell ()
handleAdvancementTab
-}

-- | Update held item in player data
handleHeldItemChange :: Server.HeldItemChange -> Civskell ()
handleHeldItemChange (Server.HeldItemChange slotNum) = overContext playerData $ playerHoldingSlot .~ slotNum

-- | Clients handle all the dirty details such that this is just a "set slot" packet
handleCreativeInventoryAction :: Server.CreativeInventoryAction -> Civskell ()
handleCreativeInventoryAction (Server.CreativeInventoryAction slotNum slotDat) = parseItemFromContext slotDat >>= \case
  Nothing -> loge $ "Unable to parse wire slot from creative inventory action: " <> showText slotDat <> ". Aborting action."
  Just s -> do
    logp $ "Player creatively set slot " <> showText slotNum <> " to {" <> ambiguously showText s <> "}"
    -- TODO: This will echo back a SetSlot packet to the client, while they should already know about the change because they initiated it. 
    -- Maybe that is ok? idk requires further testing to be sure
    ambiguously (setInventorySlot slotNum) s

{-
handleUpdateSign :: Server.UpdateSign -> Civskell ()
handleUpdateSign
-}

-- | Explicitly ignore spammy Animation packets
handleAnimation :: Server.Animation -> Civskell ()
handleAnimation (Server.Animation _anim) = pure ()

{-
handleSpectate :: Server.Spectate -> Civskell ()
handleSpectate
-}

-- | Handle clicking the block, including placement if needed.
handlePlayerBlockPlacement :: Server.PlayerBlockPlacement -> Civskell ()
handlePlayerBlockPlacement (Server.PlayerBlockPlacement block side hand cursorCoord) = do
  -- Get the block they placed on
  SuchThat (Block desc b) <- view (blockInWorld block) <$> fromContext worldData
  case onClick desc of
    Just cb -> cb b block side hand cursorCoord
    Nothing -> do
      -- Find out what item they are trying to place
      heldSlot <- slotInHand hand
      getInventorySlot heldSlot >>= \case
        -- If they right click with an empty hand, this will happen
        SuchThat (Slot Nothing) -> logp "No item to use"
        -- If they right click with a real item, check if that item has a usage continuation or consumption action
        SuchThat (Slot (Just theSlotData@(SlotData (Item idesc i) _cnt))) -> case onItemUse idesc of
          -- If they right click with an item that doesn't get used, this will happen
          Nothing -> logp "No onItemUse for item"
          -- If they right click with an item that has an action
          Just (dItem,oiu)-> do
            -- Then apply that item's "consumption" action to the inventory slot
            overContext playerData $ playerInventory . ix (fromIntegral heldSlot) .~ (ambiguate $ dItem theSlotData)
            -- And run that item's continuation
            -- This should inform the client of the above change
            oiu (Item idesc i) block side hand cursorCoord

-- | Log that they used the item, but don't do anything about it.
handleUseItem :: Server.UseItem -> Civskell ()
handleUseItem (Server.UseItem hand) = do
  usedSlot <- slotInHand hand
  usedItem <- getInventorySlot usedSlot
  logp $ "Used: " <> ambiguously showText usedItem
