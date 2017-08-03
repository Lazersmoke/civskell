{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Civskell.Packet.Clientbound where

import Crypto.Hash (hash,Digest,SHA1)
import qualified Data.Text as T
import Data.Semigroup
import Data.Functor.Identity
import Data.Bits
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Int
import Data.SuchThat
import Data.Word
import GHC.Generics
import Numeric (showHex)
import qualified Data.ByteString as BS
import Unsafe.Coerce

import Civskell.Data.Types
--import qualified Civskell.Window as Window

data LegacyHandshakePong = LegacyHandshakePong

legacyHandshakePong :: OutboundPacketDescriptor LegacyHandshakePong
legacyHandshakePong = PacketDescriptor
  {packetState = Handshaking
  ,packetName = "LegacyHandshakePong"
  ,packetPretty = const []
  ,packetHandler = defaultSerializer 0xFF
  }

instance Serial LegacyHandshakePong where
  serialize LegacyHandshakePong = putByteString legacyHandshakePongConstant
  deserialize = getByteString (BS.length legacyHandshakePongConstant) >>= \b -> if b /= legacyHandshakePongConstant then fail "Failed: deserialize @LegacyHandshakePong" else pure LegacyHandshakePong

data Disconnect = Disconnect ProtocolString deriving (Generic,Serial)

disconnect :: OutboundPacketDescriptor Disconnect
disconnect = PacketDescriptor
  {packetState = LoggingIn
  ,packetName = "Disconnect"
  ,packetPretty = \(Disconnect reason) -> [("Reason",showText . unProtocolString $ reason)]
  ,packetHandler = defaultSerializer 0x00
  }

-- Server ID, Pub Key, Verify Token
data EncryptionRequest = EncryptionRequest ProtocolString LengthAnnotatedByteString LengthAnnotatedByteString deriving (Generic,Serial)
encryptionRequest :: OutboundPacketDescriptor EncryptionRequest
encryptionRequest = PacketDescriptor
  {packetState = LoggingIn
  ,packetName = "EncryptionRequest"
  ,packetPretty = \(EncryptionRequest (ProtocolString sId) (LengthAnnotatedByteString pubKey) (LengthAnnotatedByteString vt)) -> [("Server Id",T.pack sId),("Public Key Hash",(T.take 7 $ showText (hash pubKey :: Digest SHA1)) <> "..."),("Verify Token",T.pack $ "0x" ++ (flip showHex "" =<< BS.unpack vt))]
  ,packetHandler = defaultSerializer 0x01
  }

-- UUID (with hyphens), Username
data LoginSuccess = LoginSuccess ProtocolString ProtocolString deriving (Generic,Serial)
loginSuccess :: OutboundPacketDescriptor LoginSuccess
loginSuccess = PacketDescriptor
  {packetState = LoggingIn
  ,packetName = "LoginSuccess"
  ,packetPretty = \(LoginSuccess (ProtocolString uuid) (ProtocolString name)) -> [("UUID",T.pack uuid),("Username",T.pack name)]
  ,packetHandler = defaultSerializer 0x02
  }

-- Size threshold for compression
data SetCompression = SetCompression VarInt deriving (Generic,Serial)
setCompression :: OutboundPacketDescriptor SetCompression
setCompression = PacketDescriptor
  {packetState = LoggingIn
  ,packetName = "SetCompression"
  ,packetPretty = \(SetCompression thresh) -> [("Compression Threshold",showText thresh)]
  ,packetHandler = defaultSerializer 0x03
  }

-- JSON String (for now)
data StatusResponse = StatusResponse ProtocolString deriving (Generic,Serial)
statusResponse :: OutboundPacketDescriptor StatusResponse
statusResponse = PacketDescriptor
  {packetState = Status
  ,packetName = "StatusResponse"
  -- Beware: statusJSON includes a base64 encoded png, so it is very very long
  ,packetPretty = \(StatusResponse (ProtocolString statusJSON)) -> [("Status JSON",T.pack $ take 200 statusJSON)]
  ,packetHandler = defaultSerializer 0x00
  }

-- Payload (unique number obtained from client)
data StatusPong = StatusPong Int64 deriving (Generic,Serial)
statusPong :: OutboundPacketDescriptor StatusPong
statusPong = PacketDescriptor
  {packetState = Status
  ,packetName = "StatusPong"
  ,packetPretty = \(StatusPong pongTok) -> [("Pong Token",T.pack $ "0x" ++ showHex pongTok "")]
  ,packetHandler = defaultSerializer 0x01
  }

-- EID, UUID, Type of Object, x, y, z, pitch, yaw, Object Data, velx, vely, velz
data SpawnObject = SpawnObject EntityId UUID (Some Object)
spawnObject :: OutboundPacketDescriptor SpawnObject
spawnObject = PacketDescriptor
  {packetState = Playing
  ,packetName = "SpawnObject"
  ,packetPretty = pp
  ,packetHandler = defaultSerializer 0x00
  }
  where
    pp (SpawnObject eid uuid (SuchThat (Identity (e :: o)))) = [("Entity Id",showText eid),("UUID",showText uuid),("Type",objectName @o),("Position",showText (x,y,z)),("Looking",showText (yaw,pitch)),("Data",showText d)] ++ v
      where
        v = case mVel of
          Just (EntityVelocity (dx,dy,dz)) -> [("Velocity",showText (dx,dy,dz))]
          Nothing -> []
        (d,mVel) = objectData e
        EntityLocation (x,y,z) (yaw,pitch) = objectLocation e
instance Serial SpawnObject where
  serialize (SpawnObject eid uuid (SuchThat (Identity (e :: o)))) = serialize eid *> serialize uuid *> serialize (objectId @o) *> serialize x *> serialize y *> serialize z *> serialize pitch *> serialize yaw *> dat
    where
      dat = serialize d *> (case mVel of {Just (EntityVelocity (dx,dy,dz)) -> serialize dx *> serialize dy *> serialize dz; Nothing -> serialize (0 :: Short) *> serialize (0 :: Short) *> serialize (0 :: Short)})
      (d,mVel) = objectData e
      EntityLocation (x,y,z) (yaw,pitch) = objectLocation e
  deserialize = error "Undefined: deserialize @Client.SpawnObject"

-- EID, x, y, z, count
data SpawnExpOrb = SpawnExpOrb EntityId (Double,Double,Double) Short deriving (Generic,Serial)
spawnExpOrb :: OutboundPacketDescriptor SpawnExpOrb
spawnExpOrb = PacketDescriptor
  {packetState = Playing
  ,packetName = "SpawnExpOrb"
  ,packetPretty = \(SpawnExpOrb _ _ _) -> []
  ,packetHandler = defaultSerializer 0x01
  }

-- EID, Type (always 1 for thunderbolt), x, y, z
data SpawnGlobalEntity = SpawnGlobalEntity EntityId Word8 (Double,Double,Double) deriving (Generic,Serial)
spawnGlobalEntity :: OutboundPacketDescriptor SpawnGlobalEntity
spawnGlobalEntity = PacketDescriptor
  {packetState = Playing
  ,packetName = "SpawnGlobalEntity"
  ,packetPretty = \(SpawnGlobalEntity _ _ _) -> []
  ,packetHandler = defaultSerializer 0x02
  }

-- EID, UUID, Type, x, y, z, yaw, pitch, head pitch, velx, vely, velz, Metadata
--data SpawnMob = SpawnMob EntityId UUID (Some Entity) Word8 EntityPropertySet deriving (Generic,Serial)
data SpawnMob = SpawnMob EntityId UUID VarInt (Double,Double,Double) (Word8,Word8,Word8) (Short,Short,Short) EntityPropertySet deriving (Generic,Serial)
spawnMob :: OutboundPacketDescriptor SpawnMob
spawnMob = PacketDescriptor
  {packetState = Playing
  ,packetName = "SpawnMob"
  ,packetPretty = \(SpawnMob eid uuid mobType pos look vel _props) -> [("Entity Id",showText eid),("UUID",showText uuid),("Type",showText mobType),("Position",showText pos),("Looking",showText look),("Velocity",showText vel)]
  ,packetHandler = defaultSerializer 0x03
  }

makeSpawnMob :: EntityId -> UUID -> Word8 -> Some Entity -> SpawnMob
makeSpawnMob eid uuid headPitch (SuchThat (Identity (e :: m))) = SpawnMob eid uuid (entityType @m) (x,y,z) (yaw,pitch,headPitch) (dx,dy,dz) (EntityPropertySet $ map Just $ entityMeta e)
  where
    EntityVelocity (dx,dy,dz) = entityVelocity e
    EntityLocation (x,y,z) (yaw,pitch) = entityLocation e
--instance Serial SpawnMob where
  --serialize (SpawnMob eid uuid (SuchThat (Identity (e :: m))) headPitch) = serialize eid *> serialize uuid *> serialize (entityType @m) *> serialize x *> serialize y *> serialize z *> serialize yaw *> serialize pitch *> serialize headPitch *> serialize dx *> serialize dy *> serialize dz *> serialize (0xff :: Word8)
    --where
      --EntityVelocity (dx,dy,dz) = entityVelocity e
      --EntityLocation (x,y,z) (yaw,pitch) = entityLocation e

-- EID, UUID, Title, Location,
data SpawnPainting = SpawnPainting EntityId UUID String BlockCoord Word8 deriving (Generic,Serial)
spawnPainting :: OutboundPacketDescriptor SpawnPainting
spawnPainting = PacketDescriptor
  {packetState = Playing
  ,packetName = "SpawnPainting"
  ,packetPretty = \(SpawnPainting _ _ _ _ _) -> []
  ,packetHandler = defaultSerializer 0x04
  }

-- EID, UUID, x, y, z, yaw, pitch, metadata
data SpawnPlayer = SpawnPlayer EntityId UUID (Double,Double,Double) Word8 Word8 EntityPropertySet deriving (Generic,Serial)
spawnPlayer :: OutboundPacketDescriptor SpawnPlayer
spawnPlayer = PacketDescriptor
  {packetState = Playing
  ,packetName = "SpawnPlayer"
  ,packetPretty = \(SpawnPlayer _ _ _ _ _ _) -> []
  ,packetHandler = defaultSerializer 0x05
  }

-- EID, Animation ID (from table)
data Animation = Animation EntityId Word8 deriving (Generic,Serial)
animation :: OutboundPacketDescriptor Animation
animation = PacketDescriptor
  {packetState = Playing
  ,packetName = "Animation"
  ,packetPretty = \(Animation eid anim) -> [("Entity Id",showText eid),("Animation",showText anim)]
  ,packetHandler = defaultSerializer 0x06
  }

-- List of all stats
data Statistics = Statistics (ProtocolList VarInt (ProtocolString,VarInt)) deriving (Generic,Serial)
statistics :: OutboundPacketDescriptor Statistics
statistics = PacketDescriptor
  {packetState = Playing
  ,packetName = "Statistics"
  ,packetPretty = \(Statistics _) -> []
  ,packetHandler = defaultSerializer 0x07
  }

-- EID, Block coord, stage (0-9)
data BlockBreakAnimation = BlockBreakAnimation EntityId BlockCoord Word8 deriving (Generic,Serial)
blockBreakAnimation :: OutboundPacketDescriptor BlockBreakAnimation
blockBreakAnimation = PacketDescriptor
  {packetState = Playing
  ,packetName = "BlockBreakAnimation"
  ,packetPretty = \(BlockBreakAnimation _ _ _) -> []
  ,packetHandler = defaultSerializer 0x08
  }

data UpdateBlockEntity = UpdateBlockEntity BlockCoord Word8 ProtocolNBT deriving (Generic,Serial)
updateBlockEntity :: OutboundPacketDescriptor UpdateBlockEntity
updateBlockEntity = PacketDescriptor
  {packetState = Playing
  ,packetName = "UpdateBlockEntity"
  ,packetPretty = \(UpdateBlockEntity _ _ _) -> []
  ,packetHandler = defaultSerializer 0x09
  }

-- Block coord, Action Id (enum), Action Param, Block Type ; http://wiki.vg/Block_Actions
data BlockAction = BlockAction BlockCoord (Word8,Word8) VarInt deriving (Generic,Serial)
blockAction :: OutboundPacketDescriptor BlockAction
blockAction = PacketDescriptor
  {packetState = Playing
  ,packetName = "BlockAction"
  ,packetPretty = \(BlockAction _ _ _) -> []
  ,packetHandler = defaultSerializer 0x0A
  }

-- Block coord, Block ID (from global palette)
data BlockChange = BlockChange BlockCoord (Some Block)
blockChange :: OutboundPacketDescriptor BlockChange
blockChange = PacketDescriptor
  {packetState = Playing
  ,packetName = "BlockChange"
  ,packetPretty = \(BlockChange bc bs) -> [("Block",showText bc),("New State",T.pack $ ambiguously (showBlock . runIdentity) bs)]
  ,packetHandler = defaultSerializer 0x0B
  }
instance Serial BlockChange where
  serialize (BlockChange bc bs) = serialize bc *> ambiguously (serializeBlock . runIdentity) bs
  deserialize = error "Unimplemented: deserialize @Client.BlockChange" --BlockChange <$> deserialize @BlockCoord <*> (ambiguate <$> deser

-- UUID, Action (from enum)
data BossBar = BossBar String {- BossBarAction NYI -}
bossBar :: OutboundPacketDescriptor BossBar
bossBar = PacketDescriptor
  {packetState = Playing
  ,packetName = "BossBar"
  ,packetPretty = \(BossBar _ {- BossBarAction NYI -}) -> []
  ,packetHandler = defaultSerializer 0x0C
  }
instance Serial BossBar where
  serialize (BossBar _ {- BossBarAction NYI -}) = error "Unimplemented serialize @Client.BossBar"
  deserialize = error "Unimplemented: deserialize @Client.BossBar"

-- Difficulty (0-3)
data ServerDifficulty = ServerDifficulty Difficulty deriving (Generic,Serial)
serverDifficulty :: OutboundPacketDescriptor ServerDifficulty
serverDifficulty = PacketDescriptor
  {packetState = Playing
  ,packetName = "ServerDifficulty"
  ,packetPretty = \(ServerDifficulty dif) -> [("Difficulty",showText dif)]
  ,packetHandler = defaultSerializer 0x0D
  }

-- List of matches for tab completion. Prefixed with length when sent
data TabComplete = TabComplete (ProtocolList VarInt ProtocolString) deriving (Generic,Serial)
tabComplete :: OutboundPacketDescriptor TabComplete
tabComplete = PacketDescriptor
  {packetState = Playing
  ,packetName = "TabComplete"
  ,packetPretty = \(TabComplete _) -> []
  ,packetHandler = defaultSerializer 0x0E
  }

-- JSON chat string, place to appear in (0:chatbox,1:sys msg. chatbox,2:hotbar)
data ChatMessage = ChatMessage ProtocolString Word8 deriving (Generic,Serial)
chatMessage :: OutboundPacketDescriptor ChatMessage
chatMessage = PacketDescriptor
  {packetState = Playing
  ,packetName = "ChatMessage"
  ,packetPretty = \(ChatMessage msg loc) -> [("Message",showText . unProtocolString $ msg),("Location",showText loc)]
  ,packetHandler = defaultSerializer 0x0F
  }

-- Chunk x, Chunk z, List of (chunk relative coords in special format, Block Id (global palette))
data MultiBlockChange = MultiBlockChange (Int32,Int32) (ProtocolList VarInt ((Word8,Word8),VarInt)) deriving (Generic,Serial)
multiBlockChange :: OutboundPacketDescriptor MultiBlockChange
multiBlockChange = PacketDescriptor
  {packetState = Playing
  ,packetName = "MultiBlockChange"
  ,packetPretty = \(MultiBlockChange _ _) -> []
  ,packetHandler = defaultSerializer 0x10
  }

-- Window Id, Transaction Id, Accepted
data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool deriving (Generic,Serial)
confirmTransaction :: OutboundPacketDescriptor ConfirmTransaction
confirmTransaction = PacketDescriptor
  {packetState = Playing
  ,packetName = "ConfirmTransaction"
  ,packetPretty = \(ConfirmTransaction _ _ _) -> []
  ,packetHandler = defaultSerializer 0x11
  }

data CloseWindow = CloseWindow WindowId deriving (Generic,Serial)
closeWindow :: OutboundPacketDescriptor CloseWindow
closeWindow = PacketDescriptor
  {packetState = Playing
  ,packetName = "CloseWindow"
  ,packetPretty = \(CloseWindow _) -> []
  ,packetHandler = defaultSerializer 0x12
  }

-- Window Id, Window Type (Enum), JSON chat string of Window Title, Num of Slots, optionally: EID of horse
data OpenWindow = OpenWindow WindowId (Some Window) ProtocolString (Maybe EntityId)
openWindow :: OutboundPacketDescriptor OpenWindow
openWindow = PacketDescriptor
  {packetState = Playing
  ,packetName = "OpenWindow"
  ,packetPretty = \(OpenWindow wid (SuchThat (Identity (_ :: winT))) title horseEid) -> [("Window Id",showText wid),("Window Type",windowName @winT),("Window Title",showText . unProtocolString $ title)] ++ (case horseEid of {Just eid -> [("Horse EID",showText eid)];Nothing -> []})
  ,packetHandler = defaultSerializer 0x13
  }
instance Serial OpenWindow where
  serialize (OpenWindow wid (SuchThat (Identity (_ :: winT))) title horseEid) = serialize wid *> serialize (windowIdentifier @winT) *> serialize title *> serialize ((unsafeCoerce :: Short -> Word8) $ slotCount @winT) *> (case horseEid of {Just eid -> serialize eid;Nothing -> pure ()})
  deserialize = error "Unimplemented: deserialize @Client.OpenWindow"

-- Window Id, Slots
data WindowItems = WindowItems WindowId (ProtocolList Short Slot) deriving (Generic,Serial)
windowItems :: OutboundPacketDescriptor WindowItems
windowItems = PacketDescriptor
  {packetState = Playing
  ,packetName = "WindowItems"
  ,packetPretty = \(WindowItems wid _slots) -> [("Window Id",showText wid)]
  ,packetHandler = defaultSerializer 0x14
  }
{-
 - Use this code when we write `mkWindowItems`
instance Serial WindowItems where
  -- redo
  serialize (WindowItems winId slots sc) = serialize winId *> serialize sc *> BS.concat slotList
    where
      slotList = map serialize . Map.elems $ Map.union (Map.map Just slots) fullList
      fullList = Map.fromList . map (\x -> (x,Nothing)) $ [0..sc]
-}

-- Window Id, Property (enum), Value (enum)
data WindowProperty = WindowProperty WindowId Short Short deriving (Generic,Serial)
windowProperty :: OutboundPacketDescriptor WindowProperty
windowProperty = PacketDescriptor
  {packetState = Playing
  ,packetName = "WindowProperty"
  ,packetPretty = \(WindowProperty _ _ _) -> []
  ,packetHandler = defaultSerializer 0x15
  }

-- Window Id, Slot num, <Slot>
data SetSlot = SetSlot WindowId Short Slot deriving (Generic,Serial)
setSlot :: OutboundPacketDescriptor SetSlot
setSlot = PacketDescriptor
  {packetState = Playing
  ,packetName = "SetSlot"
  ,packetPretty = \(SetSlot wid slotNum slotData) -> [("Window Id",showText wid),("Slot Number",showText slotNum),("Slot Data",showText slotData)]
  ,packetHandler = defaultSerializer 0x16
  }

-- Item Id (applies to all instances), Cooldown Ticks
data SetCooldown = SetCooldown VarInt VarInt deriving (Generic,Serial)
setCooldown :: OutboundPacketDescriptor SetCooldown
setCooldown = PacketDescriptor
  {packetState = Playing
  ,packetName = "SetCooldown"
  ,packetPretty = \(SetCooldown _ _) -> []
  ,packetHandler = defaultSerializer 0x17
  }

-- Plugin Channel, Data
data PluginMessage = PluginMessage ProtocolString BS.ByteString
pluginMessage :: OutboundPacketDescriptor PluginMessage
pluginMessage = PacketDescriptor
  {packetState = Playing
  ,packetName = "PluginMessage"
  --TODO: This hex part is very bad at indentation
  ,packetPretty = \(PluginMessage chan msg) -> [("Plugin Channel",T.pack $ unProtocolString chan),("Message",T.pack $ "0x" ++ (flip showHex "" =<< BS.unpack msg))]
  ,packetHandler = defaultSerializer 0x18
  }

-- Dont use generics because it would do the sensible thing and prepend the length.
-- Mojang doesnt do this, so we dont either
instance Serial PluginMessage where
  serialize (PluginMessage str bs) = serialize str *> putByteString bs
  deserialize = error "Cant generically deserialize a PluginMessage"

-- Sound Name (Enum), Sound Category (Enum), weird encoding for: x,y,z, Volume, Pitch
data NamedSoundEffect = NamedSoundEffect ProtocolString VarInt (Int32,Int32,Int32) Float Float deriving (Generic,Serial)
namedSoundEffect :: OutboundPacketDescriptor NamedSoundEffect
namedSoundEffect = PacketDescriptor
  {packetState = Playing
  ,packetName = "NamedSoundEffect"
  ,packetPretty = \(NamedSoundEffect _ _ _ _ _) -> []
  ,packetHandler = defaultSerializer 0x19
  }

-- Reason (JSON chat string)
data DisconnectPlay = DisconnectPlay ProtocolString deriving (Generic,Serial)
disconnectPlay :: OutboundPacketDescriptor DisconnectPlay
disconnectPlay = PacketDescriptor
  {packetState = Playing
  ,packetName = "DisconnectPlay"
  ,packetPretty = \(DisconnectPlay _) -> []
  ,packetHandler = defaultSerializer 0x1A
  }

-- EID, Status (Enum)
data EntityStatus = EntityStatus EntityId Word8 deriving (Generic,Serial)
entityStatus :: OutboundPacketDescriptor EntityStatus
entityStatus = PacketDescriptor
  {packetState = Playing
  ,packetName = "EntityStatus"
  ,packetPretty = \(EntityStatus _ _) -> []
  ,packetHandler = defaultSerializer 0x1B
  }

-- x,y,z, radius, affected block offsets, velocity of pushed player
data Explosion = Explosion (Float,Float,Float) Float [(Word8,Word8,Word8)] (Float,Float,Float) deriving (Generic,Serial)
explosion :: OutboundPacketDescriptor Explosion
explosion = PacketDescriptor
  {packetState = Playing
  ,packetName = "Explosion"
  ,packetPretty = \(Explosion _ _ _ _) -> []
  ,packetHandler = defaultSerializer 0x1C
  }

-- Chunk X, Chunk Z
data UnloadChunk = UnloadChunk (Int32,Int32) deriving (Generic,Serial)
unloadChunk :: OutboundPacketDescriptor UnloadChunk
unloadChunk = PacketDescriptor
  {packetState = Playing
  ,packetName = "UnloadChunk"
  ,packetPretty = \(UnloadChunk _) -> []
  ,packetHandler = defaultSerializer 0x1D
  }

-- Reason (Enum), Value (from Enum)
data ChangeGameState = ChangeGameState GameStateChange
changeGameState :: OutboundPacketDescriptor ChangeGameState
changeGameState = PacketDescriptor
  {packetState = Playing
  ,packetName = "ChangeGameState"
  ,packetPretty = \(ChangeGameState _) -> []
  ,packetHandler = defaultSerializer 0x1E
  }
--TODO: Lift subtype selection to type level
instance Serial ChangeGameState where
  serialize (ChangeGameState InvalidBed) = putWord8 0x00 *> serialize (0 :: Float)
  serialize (ChangeGameState (Raining isStarting)) = putWord8 (if isStarting then 0x01 else 0x02) *> serialize (0 :: Float)
  serialize (ChangeGameState (ChangeGamemode g)) = putWord8 0x03 *> serialize (case g of {Survival -> 0; Creative -> 1;} :: Float)
  serialize (ChangeGameState (ExitTheEnd showCredits)) = putWord8 0x04 *> if showCredits then serialize (1 :: Float) else serialize (0 :: Float)
  serialize (ChangeGameState DemoMessage) = putWord8 0x05 *> serialize (0 :: Float)
  serialize (ChangeGameState ArrowHitOtherPlayer) = putWord8 0x06 *> serialize (0 :: Float)
  serialize (ChangeGameState (FadeValue f)) = putWord8 0x07 *> serialize f
  serialize (ChangeGameState (FadeTime f)) = putWord8 0x08 *> serialize f
  serialize (ChangeGameState ElderGuardian) = putWord8 0x09 *> serialize (0 :: Float)
  deserialize = getWord8 >>= \case
    0x00 -> deserialize @Float >> pure (ChangeGameState InvalidBed)
    0x01 -> deserialize @Float >> pure (ChangeGameState (Raining True))
    0x02 -> deserialize @Float >> pure (ChangeGameState (Raining False))
    0x03 -> deserialize @Float >>= pure . ChangeGameState . ChangeGamemode . \case
      0 -> Survival
      1 -> Creative
      _ -> error "Invalid Gamemode in ChangeGameState"
    0x04 -> deserialize @Float >>= pure . ChangeGameState . ExitTheEnd . \case
      0 -> False
      1 -> True
      _ -> error "Invalid showCredits in ChangeGameState"
    0x05 -> deserialize @Float >> pure (ChangeGameState DemoMessage)
    0x06 -> deserialize @Float >> pure (ChangeGameState ArrowHitOtherPlayer)
    0x07 -> deserialize @Float >>= pure . ChangeGameState . FadeValue
    0x08 -> deserialize @Float >>= pure . ChangeGameState . FadeTime
    0x09 -> deserialize @Float >> pure (ChangeGameState ElderGuardian)
    _ -> error "Invalid ChangeGameState"

-- Random Id <-- Prevents Timeout
data KeepAlive = KeepAlive KeepAliveId deriving (Generic,Serial)
keepAlive :: OutboundPacketDescriptor KeepAlive
keepAlive = PacketDescriptor
  {packetState = Playing
  ,packetName = "KeepAlive"
  ,packetPretty = \(KeepAlive kid) -> [("Keep Alive Id",showText kid)]
  ,packetHandler = defaultSerializer 0x1F
  }

-- Chunk X, Chunk Z, Full Chunk?, Bitmask of slices present, [Chunk Section], optional: 256 byte array of biome data, [Block entity NBT tag]
data ChunkData = ChunkData (Int32,Int32) Bool VarInt [ChunkSection] (Maybe BS.ByteString) (ProtocolList VarInt ProtocolNBT)
chunkData :: OutboundPacketDescriptor ChunkData
chunkData = PacketDescriptor
  {packetState = Playing
  ,packetName = "ChunkData"
  ,packetPretty = \(ChunkData (cx,cz) guCont bitMask cs _mBio _nbt) -> [("Column",showText (cx,cz)),("Bit Mask",showText bitMask)] ++ (if guCont then [("Full Chunk","")] else []) ++ [("Section Count",showText (length cs))]
  ,packetHandler = defaultSerializer 0x20
  }
instance Serial ChunkData where
  serialize (ChunkData (cx,cz) guCont bitMask chunkSecs mBiomes blockEnts) = serialize cx *> serialize cz *> serialize guCont *> serialize bitMask *> withLength (runPutS $ (traverse serialize chunkSecs) *> maybe (pure ()) putByteString mBiomes) *> serialize @(ProtocolList VarInt ProtocolNBT) blockEnts
  deserialize = error "Undefined: deserialize @ChunkData"

-- Effect Id (Enum), block coord, extra data (from Enum), disable relative?
data Effect = Effect Int32 BlockCoord Int32 Bool deriving (Generic,Serial)
effect :: OutboundPacketDescriptor Effect
effect = PacketDescriptor
  {packetState = Playing
  ,packetName = "Effect"
  ,packetPretty = \(Effect _ _ _ _) -> []
  ,packetHandler = defaultSerializer 0x21
  }

data JoinGame = JoinGame EntityId Gamemode Dimension Difficulty Word8 ProtocolString Bool
joinGame :: OutboundPacketDescriptor JoinGame
joinGame = PacketDescriptor
  {packetState = Playing
  ,packetName = "JoinGame"
  ,packetPretty = \(JoinGame eid gm dim dif maxP (ProtocolString lvl) reduce) -> [("Entity Id",showText eid),("Gamemode", showText gm),("Dimension",showText dim),("Difficulty",showText dif),("Max Players",showText maxP),("Level Type",T.pack lvl)] ++ if reduce then [("Reduce Debug Info","")] else []
  ,packetHandler = defaultSerializer 0x23
  }
instance Serial JoinGame where
  -- For whatever reason, we need the eid as an Int32 here, not a VarInt
  serialize (JoinGame eid gamemode dim dif maxp leveltype reduce) = serialize (unVarInt (unEID eid)) *> serialize gamemode *> serialize dim *> serialize dif *> serialize maxp *> serialize leveltype *> serialize reduce
  deserialize = JoinGame <$> (EntityId . VarInt <$> deserialize) <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize
  
-- Flags bitfield, fly speed, fov modifier
data PlayerAbilities = PlayerAbilities AbilityFlags Float Float deriving (Generic,Serial)
playerAbilities :: OutboundPacketDescriptor PlayerAbilities
playerAbilities = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerAbilities"
  ,packetPretty = \(PlayerAbilities (AbilityFlags i f af c) flySpeed fovMod) -> u i "Invulnerable" ++ u f "Flying" ++ u af "Allow Flying" ++ u c "Creative" ++ [("Flying Speed",showText flySpeed),("FOV Modifier",showText fovMod)]
  ,packetHandler = defaultSerializer 0x2B
  }
    where
      u b s = if b then [(s,"")] else []
{-
instance Serial PlayerAbilities where
  serialize (PlayerAbilities flag fly fov) = serialize flag *> serialize fly *> serialize fov
  deserialize = PlayerAbilities <$> deserialize <*> deserialize <*> deserialize
-}

-- x,y,z, yaw,pitch, relativity flags, TPconfirm Id
data PlayerListItem a = PlayerListItem (ProtocolList VarInt (UUID,PlayerListAction a))
playerListItem :: PlayerListActionEnum a => OutboundPacketDescriptor (PlayerListItem a)
playerListItem = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerListItem"
  ,packetPretty = \(PlayerListItem (ProtocolList actions)) -> (\(u,a) -> [("UUID",showText u)] ++ showPlayerListAction a) =<< actions
  ,packetHandler = defaultSerializer 0x2D
  }

instance PlayerListActionEnum a => Serial (PlayerListItem a) where 
  serialize (PlayerListItem acts) = serialize (playerListActionEnum @a) *> serialize acts
  deserialize = error "Undefined: deserialize @PlayerListActionEnum"

data PlayerPositionAndLook = PlayerPositionAndLook (Double,Double,Double) (Float,Float) Word8 TPConfirmId
playerPositionAndLook :: OutboundPacketDescriptor PlayerPositionAndLook
playerPositionAndLook = PacketDescriptor
  {packetState = Playing
  ,packetName = "PlayerPositionAndLook"
  ,packetPretty = pp
  ,packetHandler = defaultSerializer 0x2E
  }
  where
    pp (PlayerPositionAndLook (x,y,z) (yaw,pitch) rel tid) =
      [("X",r 1 (showText x))
      ,("Y",r 2 (showText y))
      ,("Z",r 3 (showText z))
      ,("Yaw",r 4 (showText yaw))
      ,("Pitch",r 5 (showText pitch))
      ,("Teleport Id", showText tid)
      ] where r b = if testBit rel b then ("~" <>) else id
instance Serial PlayerPositionAndLook where
  serialize (PlayerPositionAndLook (x,y,z) (yaw,pitch) relFlag tpId) = serialize x *> serialize y *> serialize z *> serialize yaw *> serialize pitch *> serialize relFlag *> serialize tpId
  deserialize = PlayerPositionAndLook <$> ((,,) <$> deserialize <*> deserialize <*> deserialize) <*> ((,) <$> deserialize <*> deserialize) <*> deserialize <*> deserialize

-- Block pos of player spawn
data UpdateMetadata = UpdateMetadata EntityId EntityPropertySet
updateMetadata :: OutboundPacketDescriptor UpdateMetadata
updateMetadata = PacketDescriptor
  {packetState = Playing
  ,packetName = "UpdateMetadata"
  ,packetPretty = \(UpdateMetadata _eid _mDats) -> []
  ,packetHandler = defaultSerializer 0x39
  }
  -- TODO: subclass?
instance Serial UpdateMetadata where
  serialize (UpdateMetadata eid mDats) = serialize eid *> serialize mDats
  deserialize = UpdateMetadata <$> deserialize @EntityId <*> deserialize @EntityPropertySet

-- Block pos of player spawn
data SpawnPosition = SpawnPosition BlockCoord
spawnPosition :: OutboundPacketDescriptor SpawnPosition
spawnPosition = PacketDescriptor
  {packetState = Playing
  ,packetName = "SpawnPosition"
  ,packetPretty = \(SpawnPosition bc) -> [("Spawn",showText bc)]
  ,packetHandler = defaultSerializer 0x45
  }
instance Serial SpawnPosition where
  serialize (SpawnPosition pos) = serialize pos
  deserialize = SpawnPosition <$> deserialize @BlockCoord

-- All packets have their length and pktId annotated
-- serialize pkt = BS.append (serialize $ ,packetId @PlayerPositionAndLook) $ case pkt of

--instance Show Packet where
  --show pkt = formatPacket (packetName pkt) $ case pkt of
