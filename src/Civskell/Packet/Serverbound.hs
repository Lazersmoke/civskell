{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Civskell.Packet.Serverbound where

import Data.Int
import Control.Eff.Reader (ask)
import Control.Eff
import Control.Concurrent.STM
import Data.List (intercalate)
import Data.Functor.Identity
import Data.Word
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bytes.Serial
import Data.Semigroup
import qualified Data.Text as T
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Crypto.Hash (hash,Digest,SHA1)
import Numeric (showHex)
import Control.Monad
import Data.SuchThat
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet

import Civskell.Data.Types hiding (Player)
import Civskell.Tech.Network
import Civskell.Tech.Encrypt
import qualified Civskell.Entity as Entity
import qualified Civskell.Window as Window
--import qualified Civskell.Tile as Tile
import qualified Civskell.Block.Stone as Stone
import Civskell.Data.Player
import Civskell.Data.Logging
import Civskell.Data.World
import qualified Civskell.Packet.Clientbound as Client

  -- Protocol Version, Server Address, Server Port, Next State

parseHandshakePacket :: ParseSet
parseHandshakePacket = HashSet.fromList 
  [ambiguate handshake
  ,ambiguate legacyHandshake
  ]

parseLoginPacket :: ParseSet
parseLoginPacket = HashSet.fromList 
  [ambiguate loginStart
  ,ambiguate encryptionResponse
  ]

parseStatusPacket :: ParseSet
parseStatusPacket = HashSet.fromList 
  [ambiguate statusRequest
  ,ambiguate statusPing
  ]

-- TODO: SuchThat '[SP 'Playing] Parser
parsePlayPacket :: ParseSet
parsePlayPacket = HashSet.fromList
  [ambiguate tpConfirm
  ,ambiguate chatMessage
  ,ambiguate clientStatus
  ,ambiguate clientSettings
  ,ambiguate confirmTransaction
  ,ambiguate clickWindow
  ,ambiguate closeWindow
  ,ambiguate pluginMessage
  ,ambiguate useEntity
  ,ambiguate keepAlive
  ,ambiguate Civskell.Packet.Serverbound.playerPosition
  ,ambiguate playerPositionAndLook
  ,ambiguate playerLook
  ,ambiguate player
  ,ambiguate playerAbilities
  ,ambiguate playerDigging
  ,ambiguate entityAction
  ,ambiguate heldItemChange
  ,ambiguate creativeInventoryAction
  ,ambiguate animation
  ,ambiguate playerBlockPlacement
  ,ambiguate useItem
  ]

-- TODO: Re-specify parsers for different server states

data Handshake = Handshake VarInt ProtocolString Word16 VarInt deriving (Generic,Serial)
handshake :: PacketDescriptor Handshake
handshake = PacketDescriptor
  {packetState = Handshaking
  ,packetName = "Handshake"
  ,packetId = 0x00
  ,packetPretty = \(Handshake protocol addr port newstate) ->
    [("Protocol Version",showText protocol)
    ,("Server Address",T.pack (unProtocolString addr) <> ":" <> showText port)
    ,("Requesting change to",case newstate of {1 -> "Status"; 2 -> "Login"; _ -> "Invalid"})
    ]
  }
handleHandshake :: PacketHandler Handshake
handleHandshake = PacketHandler
  -- Normal handshake recieved
  {packetThreadingMode = ParThreading
  ,onPacket = \(Handshake protocol _addr _port newstate) -> if fromIntegral protocol == protocolVersion
    -- They are using the correct protocol version, so continue as they request
    -- TODO: Enum for newstate
    then case newstate of
      1 -> setPlayerState Status
      2 -> setPlayerState LoggingIn
      _ -> logp "Invalid newstate"
    -- They are using the incorrect protocol version. Disconnect packet will probably work even if they have a different version.
    else sendPacket Client.disconnect (Client.Disconnect (jsonyText $ "Unsupported protocol version. Please use " <> show protocolVersion))
  }

data LegacyHandshake = LegacyHandshake Word8 LegacyString Int32
legacyHandshake :: PacketDescriptor LegacyHandshake
legacyHandshake = PacketDescriptor
  {packetState = Handshaking
  ,packetName = "LegacyHandshake"
  ,packetId = 0xFE
  ,packetPretty = \(LegacyHandshake _ _ _) -> []
  }
handleLegacyHandshake :: PacketHandler LegacyHandshake
handleLegacyHandshake = PacketHandler
  -- Legacy response
  {packetThreadingMode = ParThreading
  ,onPacket = \(LegacyHandshake _ _ _) -> iSolemnlySwearIHaveNoIdeaWhatImDoing . runPutS . serialize $ Client.LegacyHandshakePong
  }

instance Serial LegacyHandshake where
  serialize (LegacyHandshake proto hostname port) = putByteString legacyHandshakePingConstant *> serialize @Int16 (fromIntegral $ 7 + BS.length hostname) *> putWord8 proto *> serialize hostname *> serialize @Int32 port
  deserialize = do
    {-guard . (==legacyHandshakePingConstant) =<<-} 
    _ <- getByteString (BS.length legacyHandshakePingConstant) 
    len <- deserialize @Int16
    _ <- lookAhead (ensure $ fromIntegral len)
    LegacyHandshake <$> getWord8 <*> getByteString (fromIntegral $ len - 7) <*> deserialize @Int32

data TPConfirm = TPConfirm VarInt deriving (Generic,Serial)
tpConfirm :: PacketDescriptor TPConfirm
tpConfirm = PacketDescriptor
  {packetState = Playing
  ,packetName = "TPConfirm"
  ,packetId = 0x00
  ,packetPretty = \(TPConfirm i) -> [("Teleport Id",showText i)]
  }
handleTPConfirm :: PacketHandler TPConfirm
handleTPConfirm = PacketHandler
  -- Check the tid presented against all the tid's we have stored
  {packetThreadingMode = ParThreading
  ,onPacket = \(TPConfirm tid) -> clearTeleport tid >>= \case
    -- If it's valid, say so
    True -> logp $ "Client confirms teleport with id: " <> showText tid
    -- If it's not, complain
    False -> loge $ "Client provided bad teleport id: " <> showText tid
  }
{-
data TabComplete = TabComplete
tabComplete :: PacketDescriptor TabComplete
tabComplete = PacketDescriptor
  {packetState = Playing
  ,packetName = "TabComplete"
  ,packetId = 0x01
  ,packetPretty = \TabComplete -> []
-}

data ChatMessage = ChatMessage ProtocolString deriving (Generic,Serial)
chatMessage  :: PacketDescriptor ChatMessage 
chatMessage  = PacketDescriptor
  {packetState = Playing
  ,packetName = "ChatMessage "
  ,packetId = 0x02
  ,packetPretty = \(ChatMessage msg) -> [("Message",T.pack $ unProtocolString msg)]
  }
handleChatMessage :: PacketHandler ChatMessage
handleChatMessage = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(ChatMessage (ProtocolString msg)) -> case msg of
    "/gamemode 1" -> setGamemode Creative
    "/gamemode 0" -> setGamemode Survival
    "chunks" -> forM_ [0..48] $ \x -> sendPacket Client.chunkData =<< colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
    "creeper" -> summonMob (Entity.Creeper Entity.defaultInsentient 0 False False)
    "/testchest" -> do
      let items = Map.fromList [{-(5,slot Item.Stick 3)-}]
      i <- send (WorldSTM $ newTVar items)
      _ <- openWindowWithItems (Window.Chest i) (jsonyText "Test Chest") i
      pure ()
    _ -> do
      broadcastPacket (Client.ChatMessage (jsonyText msg) 0)
      name <- clientUsername <$> getPlayer
      logt (T.pack name) (T.pack msg)
  }

data ClientStatus = ClientStatus ClientStatusAction deriving (Generic,Serial)
clientStatus :: PacketDescriptor ClientStatus
clientStatus = PacketDescriptor
  {packetState = Playing
  ,packetName = "ClientStatus"
  ,packetId = 0x03
  ,packetPretty = \(ClientStatus status) -> [("Status",showText status)]
  }
handleClientStatus :: PacketHandler ClientStatus
handleClientStatus = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(ClientStatus status) -> case status of
    PerformRespawn -> logp "Client wants to perform respawn"
    RequestStats -> logp "Client requests stats"
    OpenInventory -> logp "Client is opening their inventory"
  }

-- TODO: type synonyms or newtypes or whatever
data ClientSettings = ClientSettings String Word8 VarInt Bool Word8 VarInt deriving (Generic,Serial)
clientSettings :: PacketDescriptor ClientSettings
clientSettings = PacketDescriptor
  {packetState = Playing
  ,packetName = "ClientSettings"
  ,packetId = 0x04
  ,packetPretty = \(ClientSettings loc viewDist chatMode chatColors skin hand) ->
    [("Locale",T.pack loc)
    ,("View Distance",showText viewDist)
    ,("Chat Mode",showText chatMode)
    ,("Chat colors enabled",showText chatColors)
    ,("Skin bitmask",showText skin)
    ,("Main Hand",showText hand)
    ]
  }
handleClientSettings :: PacketHandler ClientSettings
handleClientSettings = PacketHandler
  -- Teleport the client when they send this packet because reasons
  -- 0x00 means all absolute (It's a relativity flag bitfield)
  {packetThreadingMode = ParThreading
  ,onPacket = \(ClientSettings _loc _viewDist _chatMode _chatColors _skin _hand) -> pendTeleport (1.0,130.0,1.0) (0.0,0.0) 0x00
  }

data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool deriving (Generic,Serial)
confirmTransaction :: PacketDescriptor ConfirmTransaction
confirmTransaction = PacketDescriptor
  {packetState = Playing
  ,packetName = "ConfirmTransaction"
  ,packetId = 0x05
  ,packetPretty = \(ConfirmTransaction wid transId acc) -> [("Window Id",showText wid),("Transaction Id",showText transId),("Accepted",if acc then "Yes" else "No")]
  }
handleConfirmTransaction :: PacketHandler ConfirmTransaction
handleConfirmTransaction = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(ConfirmTransaction wid transId _acc) -> do
    apologized <- Set.member (wid,transId) . failedTransactions <$> getPlayer
    if apologized
      then do
        logg "Client apologized for bad transaction"
        overPlayer $ \p -> p {failedTransactions = Set.delete (wid,transId) $ failedTransactions p}
      else loge "Client apologized for non-existant transaction"
  }
data EnchantItem = EnchantItem WindowId Word8 deriving (Generic,Serial)
enchantItem :: PacketDescriptor EnchantItem
enchantItem = PacketDescriptor
  {packetState = Playing
  ,packetName = "EnchantItem"
  ,packetId = 0x06
  ,packetPretty = \(EnchantItem _ _) -> []
  }

-- Slot Number
-- THIS SERIAL INSTANCE IS INCORRECT:
-- Button is in InventoryClickMode but should be before the transaction Id
data ClickWindow = ClickWindow WindowId Short TransactionId InventoryClickMode Slot
instance Serial ClickWindow where
  serialize = undefined
  deserialize = undefined
clickWindow :: PacketDescriptor ClickWindow
clickWindow = PacketDescriptor
  {packetState = Playing
  ,packetName = "ClickWindow"
  ,packetId = 0x07
  ,packetPretty = \(ClickWindow wid slotNum transId invMode item) -> [("Window Id",showText wid),("Slot Number",showText slotNum),("Transaction Id",showText transId),("Inventory Mode",showText invMode),("Subject Item",showText item)]
  }
handleClickWindow :: PacketHandler ClickWindow
handleClickWindow = PacketHandler
  -- This function needs to change items in the window Id it is given
  {packetThreadingMode = ParThreading
  ,onPacket = \(ClickWindow wid slotNum transId mode _clientProvidedSlot) -> do
    logp $ "Player clicked window " <> showText wid <> " at slot number " <> showText slotNum
    failing <- not . Set.null . failedTransactions <$> getPlayer
    (SuchThat (Identity (w :: wt))) <- flip (Map.!) wid . windows <$> getPlayer
    if failing then loge "Failed, but client is still sending clicks" else onWindowClick @wt w wid slotNum transId mode >>= sendPacket Client.confirmTransaction . Client.ConfirmTransaction wid transId
  }
  {- This code kept as reference for when we write `instance Serial ClickWindow`
   -
   - parsePacket = do
    specificVarInt 0x07 <?> "Packet Id 0x07"
    wid <- parseWID
    slotNum <- parseShort
    b <- anyWord8
    transId <- parseShort
    mode <- choice
      [guard (elem b [0,1]) *> specificVarInt 0x00 *> pure (NormalClick (b == 1))
      ,guard (elem b [0,1]) *> specificVarInt 0x01 *> pure (ShiftClick (b == 1))
      ,guard (elem b [0..8]) *> specificVarInt 0x02 *> pure (NumberKey b)
      ,guard (b == 2) *> specificVarInt 0x03 *> pure MiddleClick
      ,guard (elem b [0,1]) *> specificVarInt 0x04 *> pure (ItemDropOut (b == 1))
      ,guard (elem b [0,1,2,4,5,6,8,9,10]) *> specificVarInt 0x05 *> pure (PaintingMode b)
      ,guard (b == 0) *> specificVarInt 0x06 *> pure DoubleClick
      ]
    sl <- Item.parseSlot
    pure $ ClickWindow wid slotNum transId mode sl
  -}

data CloseWindow = CloseWindow WindowId deriving (Generic,Serial)
closeWindow :: PacketDescriptor CloseWindow
closeWindow = PacketDescriptor
  {packetState = Playing
  ,packetName = "CloseWindow"
  ,packetId = 0x08
  ,packetPretty = \(CloseWindow wid) -> [("Window Id", showText wid)]
  }
handleCloseWindow :: PacketHandler CloseWindow
handleCloseWindow = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(CloseWindow w) -> do
    logp $ "Player is closing a window with id: " <> showText w
    case w of
      0 -> pure ()
      wid -> overPlayer $ \p -> p {windows = Map.delete wid (windows p)}
  }

data PluginMessage = PluginMessage ProtocolString BS.ByteString
pluginMessage :: PacketDescriptor PluginMessage
pluginMessage = PacketDescriptor
  {packetState = Playing
  ,packetName = "PluginMessage"
  ,packetId = 0x09
  ,packetPretty = \case
    (PluginMessage "MC|Brand" cliBrand) -> [("Client Brand",showText (BS.tail cliBrand))]
    (PluginMessage chan bs) -> [("Channel",showText chan),("Payload",showText bs)]
  }
handlePluginMessage :: PacketHandler PluginMessage
handlePluginMessage = PacketHandler
  -- BS.tail removes the length prefixing
  {packetThreadingMode = ParThreading
  ,onPacket = \case
    (PluginMessage "MC|Brand" cliBrand) -> setBrand $ show (BS.tail cliBrand)
    p -> logp $ "Unsupported Plugin Message: " <> showPacket pluginMessage p
  }

instance Serial PluginMessage where
  serialize (PluginMessage ch dat) = serialize ch *> putByteString dat
  deserialize = PluginMessage <$> deserialize @ProtocolString <*> (getByteString . fromIntegral =<< remaining)

data UseEntity = UseEntity EntityId EntityInteraction deriving (Generic,Serial)
useEntity :: PacketDescriptor UseEntity
useEntity = PacketDescriptor
  {packetState = Playing
  ,packetName = "UseEntity"
  ,packetId = 0x0A
  ,packetPretty = \(UseEntity targetEID action) -> [("Target",showText targetEID),("Action",showText action)]
  }
handleUseEntity :: PacketHandler UseEntity
handleUseEntity = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(UseEntity targetEID action) -> do
    (SuchThat (Identity (_ :: m))) <- getEntity targetEID
    logg $ entityName @m <> " was " <> showText action <> "(ed)"
  }

data KeepAlive = KeepAlive KeepAliveId deriving (Generic,Serial)
keepAlive :: PacketDescriptor KeepAlive
keepAlive = PacketDescriptor
  {packetState = Playing
  ,packetName = "KeepAlive"
  ,packetId = 0x0B
  ,packetPretty = \(KeepAlive i) -> [("Keep Alive Id",showText i)]
  }
handleKeepAlive :: PacketHandler KeepAlive
handleKeepAlive = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(KeepAlive kid) -> logp $ "Player sent keep alive pong with id: " <> showText kid
  }

data PlayerPosition = PlayerPosition (Double,Double,Double) Bool deriving (Generic,Serial)
playerPosition :: PacketDescriptor PlayerPosition
playerPosition = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerPosition"
  ,packetId = 0x0C
  ,packetPretty = \(PlayerPosition (x,y,z) grounded) -> [("Positon",showText (x,y,z)),("On Ground",showText grounded)]
  }
handlePlayerPosition :: PacketHandler PlayerPosition
handlePlayerPosition = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(PlayerPosition (x,y,z) _grounded) -> setPlayerPos (x,y,z)
  }

data PlayerPositionAndLook = PlayerPositionAndLook (Double,Double,Double) (Float,Float) Bool deriving (Generic,Serial)
playerPositionAndLook :: PacketDescriptor PlayerPositionAndLook
playerPositionAndLook = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerPositionAndLook"
  ,packetId = 0x0D
  ,packetPretty = \(PlayerPositionAndLook (x,y,z) (yaw,pitch) grounded) -> [("Positon",showText (x,y,z)),("Looking",showText (yaw,pitch)),("On Ground",showText grounded)]
  }
handlePlayerPositionAndLook :: PacketHandler PlayerPositionAndLook
handlePlayerPositionAndLook = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(PlayerPositionAndLook (x,y,z) (yaw,pitch) _grounded) -> do
    setPlayerPos (x,y,z)
    setPlayerViewAngle (yaw,pitch)
  }

data PlayerLook = PlayerLook (Float,Float) Bool deriving (Generic,Serial)
playerLook :: PacketDescriptor PlayerLook
playerLook = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerLook"
  ,packetId = 0x0E
  ,packetPretty = \(PlayerLook  (yaw,pitch) grounded) -> [("Looking",showText (yaw,pitch)),("On Ground",showText grounded)]
  }
handlePlayerLook :: PacketHandler PlayerLook
handlePlayerLook = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(PlayerLook (y,p) _grounded) -> setPlayerViewAngle (y,p)
  }

data Player = Player Bool deriving (Generic,Serial)
player :: PacketDescriptor Player
player = PacketDescriptor
  {packetState = Playing
  ,packetName = "Player"
  ,packetId = 0x0F
  ,packetPretty = \(Player grounded) -> [("On Ground",showText grounded)]
  }
handlePlayer :: PacketHandler Player
handlePlayer = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(Player _grounded) -> pure ()
  }

data VehicleMove = VehicleMove (Double,Double,Double) (Float,Float) deriving (Generic,Serial)
vehicleMove :: PacketDescriptor VehicleMove
vehicleMove = PacketDescriptor
  {packetState = Playing
  ,packetName = "VehicleMove"
  ,packetId = 0x10
  ,packetPretty = \(VehicleMove _ _) -> []
  }

data SteerBoat = SteerBoat Bool Bool deriving (Generic,Serial)
steerBoat :: PacketDescriptor SteerBoat
steerBoat = PacketDescriptor
  {packetState = Playing
  ,packetName = "SteerBoat"
  ,packetId = 0x11
  ,packetPretty = \(SteerBoat _ _) -> []
  }

data PlayerAbilities = PlayerAbilities AbilityFlags Float Float deriving (Generic,Serial)
playerAbilities :: PacketDescriptor PlayerAbilities
playerAbilities = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerAbilities"
  ,packetId = 0x12
  ,packetPretty = \(PlayerAbilities (AbilityFlags i f af c) flySpeed fovMod) -> u i "Invulnerable" <> u f "Flying" <> u af "Allow Flying" <> u c "Creative" <> [("Flying Speed",showText flySpeed),("FOV Modifier",showText fovMod)]
  }
  where
    u b s = if b then [(s,"")] else []
handlePlayerAbilities :: PacketHandler PlayerAbilities
handlePlayerAbilities = PacketHandler
  -- Only sent when flight is toggled
  {packetThreadingMode = ParThreading
  ,onPacket = \(PlayerAbilities (AbilityFlags _i f _af _c) _flySpeed _fovMod) -> if f then setMoveMode Flying else setMoveMode Walking
  }

data PlayerDigging = PlayerDigging PlayerDigAction deriving (Generic,Serial)
playerDigging :: PacketDescriptor PlayerDigging
playerDigging = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerDigging"
  ,packetId = 0x13
  ,packetPretty = \(PlayerDigging action) -> case action of
    StartDig bc side -> [("Action","Start Digging"),("Block",showText bc),("Side",showText side)]
    StopDig bc side -> [("Action","Stop Digging"),("Block",showText bc),("Side",showText side)]
    EndDig bc side -> [("Action","Finished Digging"),("Block",showText bc),("Side",showText side)]
    DropItem isStack -> [("Action","Drop " <> if isStack then "Stack" else "Item")]
    ShootArrowOrFinishEating -> [("Action","inb4 Minecraft")]
    SwapHands -> [("Action","Swap items in hands")]
  }
handlePlayerDigging :: PacketHandler PlayerDigging
handlePlayerDigging = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \p@(PlayerDigging action) -> case action of
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
    _ -> loge $ "Unhandled Player Dig Action: " <> showPacket playerDigging p
  }

data EntityAction = EntityAction PlayerId PlayerEntityAction deriving (Generic,Serial)
entityAction :: PacketDescriptor EntityAction
entityAction = PacketDescriptor
  {packetState = Playing
  ,packetName = "EntityAction"
  ,packetId = 0x14
  ,packetPretty = \(EntityAction eid fire) -> (("Entity Id",showText eid):) $ let u b = if b then id else ("Stop "<>) in case fire of
    Sneak b -> [("Action",u b "Sneak")]
    Sprint b -> [("Action",u b "Sprint")]
    HorseJumpStart s -> [("Action","Horse Jump"),("Horse Jump Strength",showText s)]
    HorseJumpStop -> [("Action","Stop Horse Jump")]
    LeaveBed -> [("Action","Leave Bed")]
    HorseInventory -> [("Action","Open Horse Inventory")]
    ElytraFly -> [("Action","Elytra Fly")]
  }
handleEntityAction :: PacketHandler EntityAction
handleEntityAction = PacketHandler
  -- We know the eid because its us
  {packetThreadingMode = ParThreading
  ,onPacket = \(EntityAction _eid action) -> case action of
    Sneak True -> setMoveMode Sneaking
    Sneak False -> setMoveMode Walking
    Sprint True -> setMoveMode Sprinting
    Sprint False -> setMoveMode Walking
    HorseJumpStart _ -> logp "Jumping with horse"
    HorseJumpStop -> logp "Stopped Jumping with horse"
    LeaveBed -> pure ()
    HorseInventory -> pure () -- Open window here?
    ElytraFly -> pure () -- ?
  }

data SteerVehicle = SteerVehicle Float Float Word8 deriving (Generic,Serial)
steerVehicle :: PacketDescriptor SteerVehicle
steerVehicle = PacketDescriptor
  {packetState = Playing
  ,packetName = "SteerVehicle"
  ,packetId = 0x15
  ,packetPretty = \(SteerVehicle _ _ _) -> []
  }

data ResourcePackStatus = ResourcePackStatus VarInt deriving (Generic,Serial)
resourcePackStatus :: PacketDescriptor ResourcePackStatus
resourcePackStatus = PacketDescriptor
  {packetState = Playing
  ,packetName = "ResourcePackStatus"
  ,packetId = 0x16
  ,packetPretty = \(ResourcePackStatus _) -> []
  }

data HeldItemChange = HeldItemChange Short deriving (Generic,Serial)
heldItemChange :: PacketDescriptor HeldItemChange
heldItemChange = PacketDescriptor
  {packetState = Playing
  ,packetName = "HeldItemChange"
  ,packetId = 0x17
  ,packetPretty = \(HeldItemChange i) -> [("Slot",showText i)]
  }
handleHeldItemChange :: PacketHandler HeldItemChange
handleHeldItemChange = PacketHandler
  -- Update the slot they are holding in the player data
  {packetThreadingMode = ParThreading
  ,onPacket = \(HeldItemChange slotNum) -> setHolding slotNum
  }

data CreativeInventoryAction = CreativeInventoryAction Short Slot deriving (Generic,Serial)
creativeInventoryAction :: PacketDescriptor CreativeInventoryAction
creativeInventoryAction = PacketDescriptor
  {packetState = Playing
  ,packetName = "CreativeInventoryAction"
  ,packetId = 0x18
  ,packetPretty = \(CreativeInventoryAction slotNum item) -> [("Slot",showText slotNum),("New Item", showText item)]
  }
handleCreativeInventoryAction :: PacketHandler CreativeInventoryAction
handleCreativeInventoryAction = PacketHandler
  -- Clients handle all the dirty details such that this is just a "set slot" packet
  {packetThreadingMode = ParThreading
  ,onPacket = \(CreativeInventoryAction slotNum slotDat) -> do
    logp $ "Player creatively set slot " <> showText slotNum <> " to {" <> showText slotDat <> "}"
    -- TODO: This will echo back a SetSlot packet, add another way to access the effect inventory
    -- Maybe that is ok? idk requires further testing to be sure
    setInventorySlot slotNum slotDat
  }

data UpdateSign = UpdateSign BlockCoord (ProtocolString,ProtocolString,ProtocolString,ProtocolString) deriving (Generic,Serial)
updateSign :: PacketDescriptor UpdateSign
updateSign = PacketDescriptor
  {packetState = Playing
  ,packetName = "UpdateSign"
  ,packetId = 0x19
  ,packetPretty = \(UpdateSign _ _) -> []
  }

data Animation = Animation Hand deriving (Generic,Serial)
animation :: PacketDescriptor Animation
animation = PacketDescriptor
  {packetState = Playing
  ,packetName = "Animation"
  ,packetId = 0x1A
  ,packetPretty = \(Animation hand) -> [("Hand",showText hand)]
  }
handleAnimation :: PacketHandler Animation
handleAnimation = PacketHandler
  -- Don't do anything about the spammy animation packets
  {packetThreadingMode = ParThreading
  ,onPacket = \(Animation _anim) -> pure ()
  }

data Spectate = Spectate UUID deriving (Generic,Serial)
spectate :: PacketDescriptor Spectate
spectate = PacketDescriptor
  {packetState = Playing
  ,packetName = "Spectate"
  ,packetId = 0x1B
  ,packetPretty = \(Spectate _uuid) -> []
  }

data PlayerBlockPlacement = PlayerBlockPlacement BlockCoord BlockFace Hand (Float,Float,Float) deriving (Generic,Serial)
playerBlockPlacement :: PacketDescriptor PlayerBlockPlacement
playerBlockPlacement = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerBlockPlacement"
  ,packetId = 0x1C
  ,packetPretty = \(PlayerBlockPlacement block _side hand _cursorCoord) -> [("Block",showText block),("Hand",showText hand)]
  }
handlePlayerBlockPlacement :: PacketHandler PlayerBlockPlacement
handlePlayerBlockPlacement = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(PlayerBlockPlacement block side hand cursorCoord) -> do
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
  }

data UseItem = UseItem Hand deriving (Generic,Serial)
useItem :: PacketDescriptor UseItem
useItem = PacketDescriptor
  {packetState = Playing
  ,packetName = "UseItem"
  ,packetId = 0x1D
  ,packetPretty = \(UseItem hand) -> [("Hand",showText hand)]
  }
handleUseItem :: PacketHandler UseItem
handleUseItem = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \(UseItem hand) -> do
    -- Decide which slot they are using, and find the item in that hand
    held <- getInventorySlot =<< (case hand of {MainHand -> holdingSlot <$> getPlayer; OffHand -> pure 45})
    logp $ "Used: " <> showText held
  }

data LoginStart = LoginStart ProtocolString deriving (Generic,Serial)
loginStart :: PacketDescriptor LoginStart
loginStart = PacketDescriptor
  {packetState = LoggingIn
  ,packetName = "LoginStart"
  ,packetId = 0x00
  ,packetPretty = \(LoginStart name) -> [("Username",T.pack $ unProtocolString name)]
  -- Do the login process
  }
handleLoginStart :: PacketHandler LoginStart
handleLoginStart = PacketHandler
  -- LoginStart packets contain their username as a String
  {packetThreadingMode = ParThreading
  ,onPacket = \(LoginStart (ProtocolString name)) -> do
    -- Log that they are logging in
    logt (T.pack name) "Logging In"
    setUsername name
    -- Verify Token is fixed because why not
    -- TODO: make this a random token
    let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
    -- Server Id is blank because (((history)))
    let sId = ""
    -- Send an encryption request to the client
    sendPacket Client.encryptionRequest (Client.EncryptionRequest sId encodedPublicKey vt)
    -- TODO
    --setPlayerState (AwaitingEncryptionResponse vt sId)
  }

data EncryptionResponse = EncryptionResponse BS.ByteString BS.ByteString 
instance Serial EncryptionResponse where
  serialize (EncryptionResponse ss vt) = do
    serialize @VarInt (fromIntegral $ BS.length ss) 
    putByteString ss
    serialize @VarInt (fromIntegral $ BS.length vt) 
    putByteString vt
  deserialize = EncryptionResponse <$> (deserialize @VarInt >>= getByteString . fromIntegral) <*> (deserialize @VarInt >>= getByteString . fromIntegral)

encryptionResponse :: PacketDescriptor EncryptionResponse
encryptionResponse = PacketDescriptor
  {packetState = LoggingIn
  ,packetName = "EncryptionResponse"
  ,packetId = 0x01
  ,packetPretty = \(EncryptionResponse ss vt) ->
    [("Shared Secret Hash",(T.take 7 $ showText (hash ss :: Digest SHA1)) <> "...")
    ,("Verify Token Hash",(T.take 7 $ showText (hash vt :: Digest SHA1)) <> "...")
    ]
  }
handleEncryptionResponse :: PacketHandler EncryptionResponse
handleEncryptionResponse = PacketHandler
  -- Wait for them to send an Encryption Response
  {packetThreadingMode = ParThreading
  ,onPacket = \(EncryptionResponse ssFromClient vtFromClient) -> do
    -- TODO: get this from getPlayerState >>= \(AwaitingEcnryptionResponse vt sId) ->
    let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
    -- Make sure that the encryption stuff all lines up properly
    case checkVTandSS (snd globalKeypair) vtFromClient ssFromClient vt of
      -- If it doesn't, disconnect
      Left s -> sendPacket Client.disconnect (Client.Disconnect (jsonyText s)) >> loge (T.pack s) >> loge ("Public Key was: " <> showText (fst globalKeypair))
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
            sendPacket Client.disconnect (Client.Disconnect $ jsonyText "Auth failed (Lazersmoke's fault, probably!)")
          Just (AuthPacket uuid nameFromAuth authProps) -> do
            -- Get the config ready because we need it a lot here
            c <- ask
            pid <- registerPlayer
            setUsername nameFromAuth
            setUUID uuid
            -- Warning: setting this to 3 gave us a bad frame exception :S
            case compressionThreshold c of
              -- If the config says to compress, inform the client
              Just t -> sendPacket Client.setCompression (Client.SetCompression t) >> beginCompression t
              -- No compression -> don't do anything
              Nothing -> pure ()
            -- Send a login success. We are now in play mode
            sendPacket Client.loginSuccess (Client.LoginSuccess (show uuid) nameFromAuth)
            -- This is where the protocol specifies the state transition to be
            setPlayerState Playing
            -- 100 is the max players
            sendPacket Client.joinGame (Client.JoinGame pid (defaultGamemode c) (defaultDimension c) (defaultDifficulty c) (maxPlayers c) "default" False)
            -- Also sends player abilities
            setGamemode (defaultGamemode c)
            -- Tell them we aren't vanilla so no one gets mad at mojang for our fuck ups
            sendPacket Client.pluginMessage (Client.PluginMessage "MC|Brand" (runPutS $ serialize @ProtocolString "Civskell"))
            -- Difficulty to peaceful
            sendPacket Client.serverDifficulty (Client.ServerDifficulty (defaultDifficulty c))
            -- World Spawn/Compass Direction, not where they will spawn initially
            sendPacket Client.spawnPosition (Client.SpawnPosition (spawnLocation c))
            -- Send initial world. Need a 7x7 grid or the client gets angry with us
            forM_ [0..48] $ \x -> sendPacket Client.chunkData =<< colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
            -- Send an initial blank inventory
            sendPacket Client.windowItems (Client.WindowItems 0 (ProtocolList []))
            -- Give them some stone (for testing)
            setInventorySlot 4 (Slot . Just $ SlotData (some (Stone.Stone :: Stone.Stone 'AsItem)) 32)
            ps <- allPlayers
            sendPacket Client.playerListItem (Client.PlayerListItem (map (\p -> (clientUUID p,PlayerListAdd (clientUsername p) authProps Survival 0 Nothing)) ps))
  }

data StatusRequest = StatusRequest deriving (Generic,Serial)
statusRequest :: PacketDescriptor StatusRequest
statusRequest = PacketDescriptor
  {packetState = Status
  ,packetName = "StatusRequest"
  ,packetId = 0x00
  ,packetPretty = \StatusRequest -> []
  }
handleStatusRequest :: PacketHandler StatusRequest
handleStatusRequest = PacketHandler
  {packetThreadingMode = ParThreading
  ,onPacket = \_ -> do
    -- Get the list of connected players so we can show info about them in the server menu
    playersOnline <- allPlayers
    let userSample = intercalate "," . map (\p -> "{\"name\":\"" <> clientUsername p <> "\",\"id\":\"" <> show (clientUUID p) <> "\"}") $ playersOnline
    let resp = "{\"version\":{\"name\":\"Civskell (1.11.2)\",\"protocol\":" <> show protocolVersion <> "},\"players\":{\"max\": 100,\"online\": " <> show (length playersOnline) <> ",\"sample\":[" <> userSample <> "]},\"description\":{\"text\":\"An Experimental Minecraft server written in Haskell | github.com/Lazersmoke/civskell\"},\"favicon\":\"" <> image <> "\"}"
    -- Send resp to clients
    sendPacket Client.statusResponse (Client.StatusResponse resp)
    -- TODO
    -- setPlayerState WaitingForStatusPing
  }

data StatusPing = StatusPing Int64 deriving (Generic,Serial)
statusPing :: PacketDescriptor StatusPing
statusPing = PacketDescriptor
  {packetState = Status
  ,packetName = "StatusPing"
  ,packetId = 0x01
  ,packetPretty = \(StatusPing i) -> [("Ping Token",T.pack $ "0x" <> showHex i "")]
  }
handleStatusPing :: PacketHandler StatusPing
handleStatusPing = PacketHandler
  -- Send a pong with the same ping token right away
  {packetThreadingMode = ParThreading
  ,onPacket = \(StatusPing l) -> sendPacket Client.statusPong (Client.StatusPong l)
  }

-- TODO: make this not cancer
image :: String
image = "data:image/png;base64,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"
