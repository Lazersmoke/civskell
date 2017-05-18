{-# LANGUAGE BinaryLiterals #-}
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
instance Packet LegacyHandshakePong where
  type PacketSide LegacyHandshakePong = 'Client
  type PacketState LegacyHandshakePong = 'Handshaking
  packetName = "LegacyHandshakePong"
  packetId = 0xFF
  packetPretty _ = []
instance Serial LegacyHandshakePong where
  serialize LegacyHandshakePong = putByteString legacyHandshakePongConstant
  deserialize = getByteString (BS.length legacyHandshakePacketConstant) >>= \b -> if b /= legacyHandshakePacketConstant then fail "Failed: deserialize @LegacyHandshakePong" else pure LegacyHandshake

data Disconnect = Disconnect ProtocolString deriving (Generic,Serial)
instance Packet Disconnect where
  type PacketSide Disconnect = 'Client
  type PacketState Disconnect = 'LoggingIn
  packetName = "Disconnect"
  packetId = 0x00
  packetPretty (Disconnect reason) = [("Reason",show . unProtocolString $ reason)]

-- Server ID, Pub Key, Verify Token
data EncryptionRequest = EncryptionRequest String BS.ByteString BS.ByteString deriving (Generic,Serial)
instance Packet EncryptionRequest where
  type PacketSide EncryptionRequest = 'Client
  type PacketState EncryptionRequest = 'LoggingIn
  packetName = "EncryptionRequest"
  packetId = 0x01
  packetPretty (EncryptionRequest sId pubKey vt) = [("Server Id",sId),("Public Key Hash",(take 7 $ show (hash pubKey :: Digest SHA1)) ++ "..."),("Verify Token","0x" ++ (flip showHex "" =<< BS.unpack vt))]

-- UUID (with hyphens), Username
data LoginSuccess = LoginSuccess String String deriving (Generic,Serial)
instance Packet LoginSuccess where
  type PacketSide LoginSuccess = 'Client
  type PacketState LoginSuccess = 'LoggingIn
  packetName = "LoginSuccess"
  packetId = 0x02
  packetPretty (LoginSuccess uuid name) = [("UUID",uuid),("Username",name)]

-- Size threshold for compression
data SetCompression = SetCompression VarInt deriving (Generic,Serial)
instance Packet SetCompression where
  type PacketSide SetCompression = 'Client
  type PacketState SetCompression = 'LoggingIn
  packetName = "SetCompression"
  packetId = 0x03
  packetPretty (SetCompression thresh) = [("Compression Threshold",show thresh)]

-- JSON String (for now)
data StatusResponse = StatusResponse String deriving (Generic,Serial)
instance Packet StatusResponse where
  type PacketSide StatusResponse = 'Client
  type PacketState StatusResponse = 'Status
  packetName = "StatusResponse"
  packetId = 0x00
  -- Beware: statusJSON includes a base64 encoded png, so it is very very long
  packetPretty (StatusResponse _statusJSON) = [("Status JSON","")]

-- Payload (unique number obtained from client)
data StatusPong = StatusPong Int64 deriving (Generic,Serial)
instance Packet StatusPong where
  type PacketSide StatusPong = 'Client
  type PacketState StatusPong = 'Status
  packetName = "StatusPong"
  packetId = 0x01
  packetPretty (StatusPong pongTok) = [("Pong Token","0x" ++  showHex pongTok "")]

-- EID, UUID, Type of Object, x, y, z, pitch, yaw, Object Data, velx, vely, velz
data SpawnObject = SpawnObject EntityId UUID (Some Object)
instance Packet SpawnObject where
  type PacketSide SpawnObject = 'Client
  type PacketState SpawnObject = 'Playing
  packetName = "SpawnObject"
  packetId = 0x00
  packetPretty (SpawnObject eid uuid (SuchThat (Identity (e :: o)))) = [("Entity Id",show eid),("UUID",show uuid),("Type",objectName @o),("Position",show (x,y,z)),("Looking",show (yaw,pitch)),("Data",show d)] ++ v
    where
      v = case mVel of
        Just (EntityVelocity (dx,dy,dz)) -> [("Velocity",show (dx,dy,dz))]
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
instance Packet SpawnExpOrb where
  type PacketSide SpawnExpOrb = 'Client
  type PacketState SpawnExpOrb = 'Playing
  packetName = "SpawnExpOrb"
  packetId = 0x01
  packetPretty (SpawnExpOrb _ _ _) = []

-- EID, Type (always 1 for thunderbolt), x, y, z
data SpawnGlobalEntity = SpawnGlobalEntity EntityId Word8 (Double,Double,Double) deriving (Generic,Serial)
instance Packet SpawnGlobalEntity where
  type PacketSide SpawnGlobalEntity = 'Client
  type PacketState SpawnGlobalEntity = 'Playing
  packetName = "SpawnGlobalEntity"
  packetId = 0x02
  packetPretty (SpawnGlobalEntity _ _ _) = []

-- EID, UUID, Type, x, y, z, yaw, pitch, head pitch, velx, vely, velz, Metadata
--data SpawnMob = SpawnMob EntityId UUID (Some Entity) Word8 EntityPropertySet deriving (Generic,Serial)
data SpawnMob = SpawnMob EntityId UUID VarInt (Double,Double,Double) (Word8,Word8,Word8) (Short,Short,Short) EntityPropertySet deriving (Generic,Serial)
instance Packet SpawnMob where
  type PacketSide SpawnMob = 'Client
  type PacketState SpawnMob = 'Playing
  packetName = "SpawnMob"
  packetId = 0x03
  packetPretty (SpawnMob eid uuid mobType pos look vel _props) = [("Entity Id",show eid),("UUID",show uuid),("Type",show mobType),("Position",show pos),("Looking",show look),("Velocity",show vel)]

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
instance Packet SpawnPainting where
  type PacketSide SpawnPainting = 'Client
  type PacketState SpawnPainting = 'Playing
  packetName = "SpawnPainting"
  packetId = 0x04
  packetPretty (SpawnPainting _ _ _ _ _) = []

-- EID, UUID, x, y, z, yaw, pitch, metadata
data SpawnPlayer = SpawnPlayer EntityId UUID (Double,Double,Double) Word8 Word8 EntityPropertySet deriving (Generic,Serial)
instance Packet SpawnPlayer where
  type PacketSide SpawnPlayer = 'Client
  type PacketState SpawnPlayer = 'Playing
  packetName = "SpawnPlayer"
  packetId = 0x05
  packetPretty (SpawnPlayer _ _ _ _ _ _) = []

-- EID, Animation ID (from table)
data Animation = Animation EntityId Word8 deriving (Generic,Serial)
instance Packet Animation where
  type PacketSide Animation = 'Client
  type PacketState Animation = 'Playing
  packetName = "Animation"
  packetId = 0x06
  packetPretty (Animation eid anim) = [("Entity Id",show eid),("Animation",show anim)]

-- List of all stats
data Statistics = Statistics (ProtocolList VarInt (ProtocolString,VarInt)) deriving (Generic,Serial)
instance Packet Statistics where
  type PacketSide Statistics = 'Client
  type PacketState Statistics = 'Playing
  packetName = "Statistics"
  packetId = 0x07
  packetPretty (Statistics _) = []

-- EID, Block coord, stage (0-9)
data BlockBreakAnimation = BlockBreakAnimation EntityId BlockCoord Word8 deriving (Generic,Serial)
instance Packet BlockBreakAnimation where
  type PacketSide BlockBreakAnimation = 'Client
  type PacketState BlockBreakAnimation = 'Playing
  packetName = "BlockBreakAnimation"
  packetId = 0x08
  packetPretty (BlockBreakAnimation _ _ _) = []

data UpdateBlockEntity = UpdateBlockEntity BlockCoord Word8 ProtocolNBT deriving (Generic,Serial)
instance Packet UpdateBlockEntity where
  type PacketSide UpdateBlockEntity = 'Client
  type PacketState UpdateBlockEntity = 'Playing
  packetName = "UpdateBlockEntity"
  packetId = 0x09
  packetPretty (UpdateBlockEntity _ _ _) = []

-- Block coord, Action Id (enum), Action Param, Block Type ; http://wiki.vg/Block_Actions
data BlockAction = BlockAction BlockCoord (Word8,Word8) VarInt deriving (Generic,Serial)
instance Packet BlockAction where
  type PacketSide BlockAction = 'Client
  type PacketState BlockAction = 'Playing
  packetName = "BlockAction"
  packetId = 0x0A
  packetPretty (BlockAction _ _ _) = []

-- Block coord, Block ID (from global palette)
data BlockChange = BlockChange BlockCoord (Some Block)
instance Packet BlockChange where
  type PacketSide BlockChange = 'Client
  type PacketState BlockChange = 'Playing
  packetName = "BlockChange"
  packetId = 0x0B
  packetPretty (BlockChange bc bs) = [("Block",show bc),("New State",ambiguously (showBlock . runIdentity) bs)]
instance Serial BlockChange where
  serialize (BlockChange bc bs) = serialize bc *> ambiguously (serializeBlock . runIdentity) bs
  deserialize = error "Unimplemented: deserialize @Client.BlockChange" --BlockChange <$> deserialize @BlockCoord <*> (ambiguate <$> deser

-- UUID, Action (from enum)
data BossBar = BossBar String {- BossBarAction NYI -}
instance Packet BossBar where
  type PacketSide BossBar = 'Client
  type PacketState BossBar = 'Playing
  packetName = "BossBar"
  packetId = 0x0C
  packetPretty (BossBar _ {- BossBarAction NYI -}) = []
instance Serial BossBar where
  serialize (BossBar _ {- BossBarAction NYI -}) = error "Unimplemented serialize @Client.BossBar"
  deserialize = error "Unimplemented: deserialize @Client.BossBar"

-- Difficulty (0-3)
data ServerDifficulty = ServerDifficulty Difficulty deriving (Generic,Serial)
instance Packet ServerDifficulty where
  type PacketSide ServerDifficulty = 'Client
  type PacketState ServerDifficulty = 'Playing
  packetName = "ServerDifficulty"
  packetId = 0x0D
  packetPretty (ServerDifficulty dif) = [("Difficulty",show dif)]

-- List of matches for tab completion. Prefixed with length when sent
data TabComplete = TabComplete (ProtocolList VarInt ProtocolString)
instance Packet TabComplete where
  type PacketSide TabComplete = 'Client
  type PacketState TabComplete = 'Playing
  packetName = "TabComplete"
  packetId = 0x0E
  packetPretty (TabComplete _) = []

-- JSON chat string, place to appear in (0:chatbox,1:sys msg. chatbox,2:hotbar)
data ChatMessage = ChatMessage ProtocolString Word8 deriving (Generic,Serial)
instance Packet ChatMessage where
  type PacketSide ChatMessage = 'Client
  type PacketState ChatMessage = 'Playing
  packetName = "ChatMessage"
  packetId = 0x0F
  packetPretty (ChatMessage msg loc) = [("Message",show . unProtocolString $ msg),("Location",show loc)]

-- Chunk x, Chunk z, List of (chunk relative coords in special format, Block Id (global palette))
data MultiBlockChange = MultiBlockChange (Int32,Int32) (ProtocolList VarInt ((Word8,Word8),VarInt)) deriving (Generic,Serial)
instance Packet MultiBlockChange where
  type PacketSide MultiBlockChange = 'Client
  type PacketState MultiBlockChange = 'Playing
  packetName = "MultiBlockChange"
  packetId = 0x10
  packetPretty (MultiBlockChange _ _) = []

-- Window Id, Transaction Id, Accepted
data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool deriving (Generic,Serial)
instance Packet ConfirmTransaction where
  type PacketSide ConfirmTransaction = 'Client
  type PacketState ConfirmTransaction = 'Playing
  packetName = "ConfirmTransaction"
  packetId = 0x11
  packetPretty (ConfirmTransaction _ _ _) = []

data CloseWindow = CloseWindow WindowId deriving (Generic,Serial)
instance Packet CloseWindow where
  type PacketSide CloseWindow = 'Client
  type PacketState CloseWindow = 'Playing
  packetName = "CloseWindow"
  packetId = 0x12
  packetPretty (CloseWindow _) = []

-- Window Id, Window Type (Enum), JSON chat string of Window Title, Num of Slots, optionally: EID of horse
data OpenWindow = OpenWindow WindowId (Some Window) ProtocolString (Maybe EntityId)
instance Packet OpenWindow where
  type PacketSide OpenWindow = 'Client
  type PacketState OpenWindow = 'Playing
  packetName = "OpenWindow"
  packetId = 0x13
  packetPretty (OpenWindow wid (SuchThat (Identity (_ :: winT))) title horseEid) = [("Window Id",show wid),("Window Type",windowName @winT),("Window Title",show . unProtocolString $ title)] ++ (case horseEid of {Just eid -> [("Horse EID",show eid)];Nothing -> []})
instance Serial OpenWindow where
  serialize (OpenWindow wid (SuchThat (Identity (_ :: winT))) title horseEid) = serialize wid *> serialize (windowIdentifier @winT) *> serialize title *> serialize ((unsafeCoerce :: Short -> Word8) $ slotCount @winT) *> (case horseEid of {Just eid -> serialize eid;Nothing -> pure ()})
  deserialize = error "Unimplemented: deserialize @Client.OpenWindow"

-- Window Id, Slots
data WindowItems = WindowItems WindowId (ProtocolList Short Slot) deriving (Generic,Serial)
instance Packet WindowItems where
  type PacketSide WindowItems = 'Client
  type PacketState WindowItems = 'Playing
  packetName = "WindowItems"
  packetId = 0x14
  packetPretty (WindowItems wid _slots) = [("Window Id",show wid)]
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
instance Packet WindowProperty where
  type PacketSide WindowProperty = 'Client
  type PacketState WindowProperty = 'Playing
  packetName = "WindowProperty"
  packetId = 0x15
  packetPretty (WindowProperty _ _ _) = []

-- Window Id, Slot num, <Slot>
data SetSlot = SetSlot WindowId Short Slot deriving (Generic,Serial)
instance Packet SetSlot where
  type PacketSide SetSlot = 'Client
  type PacketState SetSlot = 'Playing
  packetName = "SetSlot"
  packetId = 0x16
  packetPretty (SetSlot wid slotNum slotData) = [("Window Id",show wid),("Slot Number",show slotNum),("Slot Data",show slotData)]

-- Item Id (applies to all instances), Cooldown Ticks
data SetCooldown = SetCooldown VarInt VarInt deriving (Generic,Serial)
instance Packet SetCooldown where
  type PacketSide SetCooldown = 'Client
  type PacketState SetCooldown = 'Playing
  packetName = "SetCooldown"
  packetId = 0x17
  packetPretty (SetCooldown _ _) = []

-- Plugin Channel, Data
data PluginMessage = PluginMessage ProtocolString BS.ByteString
instance Packet PluginMessage where
  type PacketSide PluginMessage = 'Client
  type PacketState PluginMessage = 'Playing
  packetName = "PluginMessage"
  packetId = 0x18
  --TODO: This hex part is very bad at indentation
  packetPretty (PluginMessage chan msg) = [("Plugin Channel",unProtocolString chan),("Message","0x" ++ (flip showHex "" =<< BS.unpack msg))]

-- Don't use generics because it would do the sensible thing and prepend the length.
-- Mojang doesn't do this, so we don't either
instance Serial PluginMessage where
  serialize (PluginMessage str bs) = serialize str *> putByteString bs
  deserialize = error "Can't generically deserialize a PluginMessage"

-- Sound Name (Enum), Sound Category (Enum), weird encoding for: x,y,z, Volume, Pitch
data NamedSoundEffect = NamedSoundEffect ProtocolString VarInt (Int32,Int32,Int32) Float Float deriving (Generic,Serial)
instance Packet NamedSoundEffect where
  type PacketSide NamedSoundEffect = 'Client
  type PacketState NamedSoundEffect = 'Playing
  packetName = "NamedSoundEffect"
  packetId = 0x19
  packetPretty (NamedSoundEffect _ _ _ _ _) = []

-- Reason (JSON chat string)
data DisconnectPlay = DisconnectPlay ProtocolString deriving (Generic,Serial)
instance Packet DisconnectPlay where
  type PacketSide DisconnectPlay = 'Client
  type PacketState DisconnectPlay = 'Playing
  packetName = "DisconnectPlay"
  packetId = 0x1A
  packetPretty (DisconnectPlay _) = []

-- EID, Status (Enum)
data EntityStatus = EntityStatus EntityId Word8 deriving (Generic,Serial)
instance Packet EntityStatus where
  type PacketSide EntityStatus = 'Client
  type PacketState EntityStatus = 'Playing
  packetName = "EntityStatus"
  packetId = 0x1B
  packetPretty (EntityStatus _ _) = []

-- x,y,z, radius, affected block offsets, velocity of pushed player
data Explosion = Explosion (Float,Float,Float) Float [(Word8,Word8,Word8)] (Float,Float,Float) deriving (Generic,Serial)
instance Packet Explosion where
  type PacketSide Explosion = 'Client
  type PacketState Explosion = 'Playing
  packetName = "Explosion"
  packetId = 0x1C
  packetPretty (Explosion _ _ _ _) = []

-- Chunk X, Chunk Z
data UnloadChunk = UnloadChunk (Int32,Int32) deriving (Generic,Serial)
instance Packet UnloadChunk where
  type PacketSide UnloadChunk = 'Client
  type PacketState UnloadChunk = 'Playing
  packetName = "UnloadChunk"
  packetId = 0x1D
  packetPretty (UnloadChunk _) = []

-- Reason (Enum), Value (from Enum)
data ChangeGameState = ChangeGameState GameStateChange
instance Packet ChangeGameState where
  type PacketSide ChangeGameState = 'Client
  type PacketState ChangeGameState = 'Playing
  packetName = "ChangeGameState"
  packetId = 0x1E
  packetPretty (ChangeGameState _) = []
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
instance Packet KeepAlive where
  type PacketSide KeepAlive = 'Client
  type PacketState KeepAlive = 'Playing
  packetName = "KeepAlive"
  packetId = 0x1F
  packetPretty (KeepAlive kid) = [("Keep Alive Id",show kid)]

-- Chunk X, Chunk Z, Full Chunk?, Bitmask of slices present, [Chunk Section], optional: 256 byte array of biome data, [Block entity NBT tag]
data ChunkData = ChunkData (Int32,Int32) Bool VarInt [ChunkSection] (Maybe BS.ByteString) (ProtocolList VarInt ProtocolNBT)
instance Packet ChunkData where
  type PacketSide ChunkData = 'Client
  type PacketState ChunkData = 'Playing
  packetName = "ChunkData"
  packetId = 0x20
  packetPretty (ChunkData (cx,cz) guCont bitMask cs _mBio _nbt) = [("Column",show (cx,cz)),("Bit Mask",show bitMask)] ++ (if guCont then [("Full Chunk","")] else []) ++ [("Section Count",show (length cs))]
instance Serial ChunkData where
  serialize (ChunkData (cx,cz) guCont bitMask chunkSecs mBiomes blockEnts) = serialize cx *> serialize cz *> serialize guCont *> serialize bitMask *> withLength (runPutS $ (traverse serialize chunkSecs) *> maybe (pure ()) putByteString mBiomes) *> serialize @(ProtocolList VarInt ProtocolNBT) blockEnts
  deserialize = error "Undefined: deserialize @ChunkData"

-- Effect Id (Enum), block coord, extra data (from Enum), disable relative?
data Effect = Effect Int32 BlockCoord Int32 Bool deriving (Generic,Serial)
instance Packet Effect where
  type PacketSide Effect = 'Client
  type PacketState Effect = 'Playing
  packetName = "Effect"
  packetId = 0x21
  packetPretty (Effect _ _ _ _) = []

data JoinGame = JoinGame EntityId Gamemode Dimension Difficulty Word8 String Bool
instance Packet JoinGame where
  type PacketSide JoinGame = 'Client
  type PacketState JoinGame = 'Playing
  packetName = "JoinGame"
  packetId = 0x23
  packetPretty (JoinGame eid gm dim dif maxP lvl reduce) = [("Entity Id",show eid),("Gamemode", show gm),("Dimension",show dim),("Difficulty",show dif),("Max Players",show maxP),("Level Type",lvl)] ++ if reduce then [("Reduce Debug Info","")] else []
instance Serial JoinGame where
  -- For whatever reason, we need the eid as an Int32 here, not a VarInt
  serialize (JoinGame eid gamemode dim dif maxp leveltype reduce) = serialize (unVarInt (unEID eid)) *> serialize gamemode *> serialize dim *> serialize dif *> serialize maxp *> serialize leveltype *> serialize reduce
  deserialize = JoinGame <$> (EntityId . VarInt <$> deserialize) <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize
  
-- Flags bitfield, fly speed, fov modifier
data PlayerAbilities = PlayerAbilities AbilityFlags Float Float deriving (Generic,Serial)
instance Packet PlayerAbilities where
  type PacketSide PlayerAbilities = 'Client
  type PacketState PlayerAbilities = 'Playing
  packetName = "PlayerAbilities"
  packetId = 0x2B
  packetPretty (PlayerAbilities (AbilityFlags i f af c) flySpeed fovMod) = u i "Invulnerable" ++ u f "Flying" ++ u af "Allow Flying" ++ u c "Creative" ++ [("Flying Speed",show flySpeed),("FOV Modifier",show fovMod)]
    where
      u b s = if b then [(s,"")] else []
{-
instance Serial PlayerAbilities where
  serialize (PlayerAbilities flag fly fov) = serialize flag *> serialize fly *> serialize fov
  deserialize = PlayerAbilities <$> deserialize <*> deserialize <*> deserialize
-}

-- x,y,z, yaw,pitch, relativity flags, TPconfirm Id
data PlayerListItem a = PlayerListItem [(UUID,PlayerListAction a)]
instance Packet (PlayerListItem a) where
  type PacketSide (PlayerListItem a) = 'Client
  type PacketState (PlayerListItem a) = 'Playing
  packetName = "PlayerListItem"
  packetId = 0x2D
  packetPretty (PlayerListItem actions) = (\(u,_) -> [("UUID",show u),("Action",show "")]) =<< actions

instance PlayerListActionEnum a => Serial (PlayerListItem a) where 
  serialize (PlayerListItem acts) = serialize (playerListActionEnum @a) *> withListLength acts
  deserialize = error "Undefined: deserialize @PlayerListActionEnum"

data PlayerPositionAndLook = PlayerPositionAndLook (Double,Double,Double) (Float,Float) Word8 TPConfirmId
instance Packet PlayerPositionAndLook where
  type PacketSide PlayerPositionAndLook = 'Client
  type PacketState PlayerPositionAndLook = 'Playing
  packetName = "PlayerPositionAndLook"
  packetId = 0x2E
  packetPretty (PlayerPositionAndLook (x,y,z) (yaw,pitch) rel tid) =
    [("X",r 1 (show x))
    ,("Y",r 2 (show y))
    ,("Z",r 3 (show z))
    ,("Yaw",r 4 (show yaw))
    ,("Pitch",r 5 (show pitch))
    ,("Teleport Id", show tid)
    ]
    where
      r b = if testBit rel b then ("~" ++) else id
instance Serial PlayerPositionAndLook where
  serialize (PlayerPositionAndLook (x,y,z) (yaw,pitch) relFlag tpId) = serialize x *> serialize y *> serialize z *> serialize yaw *> serialize pitch *> serialize relFlag *> serialize tpId
  deserialize = PlayerPositionAndLook <$> ((,,) <$> deserialize <*> deserialize <*> deserialize) <*> ((,) <$> deserialize <*> deserialize) <*> deserialize <*> deserialize

-- Block pos of player spawn
data UpdateMetadata = UpdateMetadata EntityId EntityPropertySet
instance Packet UpdateMetadata where
  type PacketSide UpdateMetadata = 'Client
  type PacketState UpdateMetadata = 'Playing
  packetName = "UpdateMetadata"
  packetId = 0x39
  packetPretty (UpdateMetadata _eid _mDats) = []
  -- TODO: subclass?
instance Serial UpdateMetadata where
  serialize (UpdateMetadata eid mDats) = serialize eid *> serialize mDats
  deserialize = UpdateMetadata <$> deserialize @EntityId <*> deserialize @EntityPropertySet

-- Block pos of player spawn
data SpawnPosition = SpawnPosition BlockCoord
instance Packet SpawnPosition where
  type PacketSide SpawnPosition = 'Client
  type PacketState SpawnPosition = 'Playing
  packetName = "SpawnPosition"
  packetId = 0x43
  packetPretty (SpawnPosition bc) = [("Spawn",show bc)]
instance Serial SpawnPosition where
  serialize (SpawnPosition pos) = serialize pos
  deserialize = SpawnPosition <$> deserialize @BlockCoord

-- All packets have their length and pktId annotated
-- serialize pkt = BS.append (serialize $ packetId @PlayerPositionAndLook) $ case pkt of

--instance Show Packet where
  --show pkt = formatPacket (packetName pkt) $ case pkt of
