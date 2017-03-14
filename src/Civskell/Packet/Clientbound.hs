{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Civskell.Packet.Clientbound where

import Crypto.Hash (hash,Digest,SHA1)
import Data.Functor.Identity
import Data.Bits
import Data.Int
import Data.List (genericLength)
import Data.Maybe
import Data.SuchThat
import Data.NBT
import Data.Semigroup
import Data.Word
import Numeric (showHex)
import qualified Data.ByteString as BS

import Civskell.Data.Types hiding (Player)

  -- Login
  -- Reason

data Disconnect = Disconnect String
instance Packet Disconnect where
  type PacketSide Disconnect = 'Client
  type PacketState Disconnect = 'LoggingIn
  packetName = "Disconnect"
  packetId = 0x00
  packetPretty (Disconnect reason) = [("Reason",reason)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize Disconnect where
  serialize (Disconnect reason) = serialize reason
  -- Server ID, Pub Key, Verify Token

data EncryptionRequest = EncryptionRequest String BS.ByteString BS.ByteString
instance Packet EncryptionRequest where
  type PacketSide EncryptionRequest = 'Client
  type PacketState EncryptionRequest = 'LoggingIn
  packetName = "EncryptionRequest"
  packetId = 0x01
  packetPretty (EncryptionRequest sId pubKey vt) = [("Server Id",sId),("Public Key Hash",(take 7 $ show (hash pubKey :: Digest SHA1)) ++ "..."),("Verify Token","0x" ++ (flip showHex "" =<< BS.unpack vt))]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize EncryptionRequest where
  serialize (EncryptionRequest sId p vt) = serialize sId <> withLength p <> withLength vt
  -- UUID (with hyphens), Username

data LoginSuccess = LoginSuccess String String
instance Packet LoginSuccess where
  type PacketSide LoginSuccess = 'Client
  type PacketState LoginSuccess = 'LoggingIn
  packetName = "LoginSuccess"
  packetId = 0x02
  packetPretty (LoginSuccess uuid name) = [("UUID",uuid),("Username",name)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize LoginSuccess where
  serialize (LoginSuccess uuid name) = serialize uuid <> serialize name
  -- Size threshold for compression

data SetCompression = SetCompression VarInt
instance Packet SetCompression where
  type PacketSide SetCompression = 'Client
  type PacketState SetCompression = 'LoggingIn
  packetName = "SetCompression"
  packetId = 0x03
  packetPretty (SetCompression thresh) = [("Compresion Threshold",show thresh)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SetCompression where
  serialize (SetCompression thresh) = serialize thresh
  -- Status
  -- JSON String (for now)

data StatusResponse = StatusResponse String
instance Packet StatusResponse where
  type PacketSide StatusResponse = 'Client
  type PacketState StatusResponse = 'Status
  packetName = "StatusResponse"
  packetId = 0x00
  -- Beware: statusJSON includes a base64 encoded png, so it is very very long
  packetPretty (StatusResponse _statusJSON) = [("Status JSON","")]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize StatusResponse where
  serialize (StatusResponse s) = serialize s

-- Payload (unique number obtained from client)
data StatusPong = StatusPong Int64
instance Packet StatusPong where
  type PacketSide StatusPong = 'Client
  type PacketState StatusPong = 'Status
  packetName = "StatusPong"
  packetId = 0x01
  packetPretty (StatusPong pongTok) = [("Pong Token","0x" ++  showHex pongTok "")]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize StatusPong where
  serialize (StatusPong l) = serialize l

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
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SpawnObject where
  serialize (SpawnObject eid uuid (SuchThat (Identity (e :: o)))) = serialize eid <> serialize uuid <> serialize (objectId @o) <> serialize x <> serialize y <> serialize z <> serialize pitch <> serialize yaw <> dat
    where
      dat = serialize d <> (case mVel of {Just (EntityVelocity (dx,dy,dz)) -> serialize dx <> serialize dy <> serialize dz; Nothing -> serialize (0 :: Short) <> serialize (0 :: Short) <> serialize (0 :: Short)})
      (d,mVel) = objectData e
      EntityLocation (x,y,z) (yaw,pitch) = objectLocation e
  -- EID, x, y, z, count

data SpawnExpOrb = SpawnExpOrb EntityId (Double,Double,Double) Short
instance Packet SpawnExpOrb where
  type PacketSide SpawnExpOrb = 'Client
  type PacketState SpawnExpOrb = 'Playing
  packetName = "SpawnExpOrb"
  packetId = 0x01
  packetPretty (SpawnExpOrb _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SpawnExpOrb where
  serialize (SpawnExpOrb _ _ _) = error "Unimplemented Serialization"
  -- EID, Type (always 1 for thunderbolt), x, y, z

data SpawnGlobalEntity = SpawnGlobalEntity EntityId Word8 (Double,Double,Double)
instance Packet SpawnGlobalEntity where
  type PacketSide SpawnGlobalEntity = 'Client
  type PacketState SpawnGlobalEntity = 'Playing
  packetName = "SpawnGlobalEntity"
  packetId = 0x02
  packetPretty (SpawnGlobalEntity _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SpawnGlobalEntity where
  serialize (SpawnGlobalEntity _ _ _) = error "Unimplemented Serialization"
  -- EID, UUID, Type, x, y, z, yaw, pitch, head pitch, velx, vely, velz, Metadata

data SpawnMob = SpawnMob EntityId UUID (Some Entity) Word8
instance Packet SpawnMob where
  type PacketSide SpawnMob = 'Client
  type PacketState SpawnMob = 'Playing
  packetName = "SpawnMob"
  packetId = 0x03
  packetPretty (SpawnMob eid uuid (SuchThat (Identity (e :: m))) headPitch) = [("Entity Id",show eid),("UUID",show uuid),("Type",entityName @m),("Position",show (x,y,z)),("Looking",show (yaw,pitch,headPitch)),("Velocity",show (dx,dy,dz))]
    where
      EntityVelocity (dx,dy,dz) = entityVelocity e
      EntityLocation (x,y,z) (yaw,pitch) = entityLocation e
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SpawnMob where
  serialize (SpawnMob eid uuid (SuchThat (Identity (e :: m))) headPitch) = serialize eid <> serialize uuid <> serialize (entityType @m) <> serialize x <> serialize y <> serialize z <> serialize yaw <> serialize pitch <> serialize headPitch <> serialize dx <> serialize dy <> serialize dz <> serialize (0xff :: Word8)
    where
      EntityVelocity (dx,dy,dz) = entityVelocity e
      EntityLocation (x,y,z) (yaw,pitch) = entityLocation e
  -- EID, UUID, Title, Location,

data SpawnPainting = SpawnPainting EntityId String String {-Position NYI-} Word8
instance Packet SpawnPainting where
  type PacketSide SpawnPainting = 'Client
  type PacketState SpawnPainting = 'Playing
  packetName = "SpawnPainting"
  packetId = 0x04
  packetPretty (SpawnPainting _ _ _ {-NYI-} _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SpawnPainting where
  serialize (SpawnPainting _ _ _ {-NYI-} _) = error "Unimplemented Serialization"
  -- EID, UUID, x, y, z, yaw, pitch, metadata

data SpawnPlayer = SpawnPlayer EntityId String (Double,Double,Double) Word8 Word8
instance Packet SpawnPlayer where
  type PacketSide SpawnPlayer = 'Client
  type PacketState SpawnPlayer = 'Playing
  packetName = "SpawnPlayer"
  packetId = 0x05
  packetPretty (SpawnPlayer _ _ _ _ _ {-Metadata NYI-}) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SpawnPlayer where
  serialize (SpawnPlayer _ _ _ _ _ {-Metadata NYI-}) = error "Unimplemented Serialization"
  -- EID, Animation ID (from table)

data Animation = Animation EntityId Word8
instance Packet Animation where
  type PacketSide Animation = 'Client
  type PacketState Animation = 'Playing
  packetName = "Animation"
  packetId = 0x06
  packetPretty (Animation eid anim) = [("Entity Id",show eid),("Animation",show anim)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize Animation where
  serialize (Animation eid anim) = serialize eid <> serialize anim
  -- List of all stats

data Statistics = Statistics [(String,VarInt)]
instance Packet Statistics where
  type PacketSide Statistics = 'Client
  type PacketState Statistics = 'Playing
  packetName = "Statistics"
  packetId = 0x07
  packetPretty (Statistics _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize Statistics where
  serialize (Statistics _) = error "Unimplemented Serialization"
  -- EID, Block coord, stage (0-9)

data BlockBreakAnimation = BlockBreakAnimation EntityId BlockCoord Word8
instance Packet BlockBreakAnimation where
  type PacketSide BlockBreakAnimation = 'Client
  type PacketState BlockBreakAnimation = 'Playing
  packetName = "BlockBreakAnimation"
  packetId = 0x08
  packetPretty (BlockBreakAnimation _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize BlockBreakAnimation where
  serialize (BlockBreakAnimation _ _ _) = error "Unimplemented Serialization"

data UpdateBlockEntity = UpdateBlockEntity BlockCoord Word8 BS.ByteString
instance Packet UpdateBlockEntity where
  type PacketSide UpdateBlockEntity = 'Client
  type PacketState UpdateBlockEntity = 'Playing
  packetName = "UpdateBlockEntity"
  packetId = 0x09
  packetPretty (UpdateBlockEntity _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize UpdateBlockEntity where
  serialize (UpdateBlockEntity _ _ _) = error "Unimplemented Serialization"
  -- Block coord, Action Id (enum), Action Param, Block Type ; http://wiki.vg/Block_Actions

data BlockAction = BlockAction BlockCoord (Word8,Word8) VarInt
instance Packet BlockAction where
  type PacketSide BlockAction = 'Client
  type PacketState BlockAction = 'Playing
  packetName = "BlockAction"
  packetId = 0x0A
  packetPretty (BlockAction _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize BlockAction where
  serialize (BlockAction _ _ _) = error "Unimplemented Serialization"
  -- Block coord, Block ID (from global palette)

data BlockChange = BlockChange BlockCoord BlockState
instance Packet BlockChange where
  type PacketSide BlockChange = 'Client
  type PacketState BlockChange = 'Playing
  packetName = "BlockChange"
  packetId = 0x0B
  packetPretty (BlockChange bc bs) = [("Block",show bc),("New State",show bs)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize BlockChange where
  serialize (BlockChange block bs) = serialize block <> serialize bs
  -- UUID, Action (from enum)

data BossBar = BossBar String {- BossBarAction NYI -}
instance Packet BossBar where
  type PacketSide BossBar = 'Client
  type PacketState BossBar = 'Playing
  packetName = "BossBar"
  packetId = 0x0C
  packetPretty (BossBar _ {- BossBarAction NYI -}) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize BossBar where
  serialize (BossBar _ {- BossBarAction NYI -}) = error "Unimplemented Serialization"
  -- Difficulty (0-3)

data ServerDifficulty = ServerDifficulty Difficulty
instance Packet ServerDifficulty where
  type PacketSide ServerDifficulty = 'Client
  type PacketState ServerDifficulty = 'Playing
  packetName = "ServerDifficulty"
  packetId = 0x0D
  packetPretty (ServerDifficulty dif) = [("Difficulty",show dif)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize ServerDifficulty where
  serialize (ServerDifficulty dif) = serialize dif
  -- List of matches for tab completion. Prefixed with length when sent

data TabComplete = TabComplete [String]
instance Packet TabComplete where
  type PacketSide TabComplete = 'Client
  type PacketState TabComplete = 'Playing
  packetName = "TabComplete"
  packetId = 0x0E
  packetPretty (TabComplete _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize TabComplete where
  serialize (TabComplete _) = error "Unimplemented Serialization"
  -- JSON chat string, place to appear in (0:chatbox,1:sys msg. chatbox,2:hotbar)

data ChatMessage = ChatMessage String Word8
instance Packet ChatMessage where
  type PacketSide ChatMessage = 'Client
  type PacketState ChatMessage = 'Playing
  packetName = "ChatMessage"
  packetId = 0x0F
  packetPretty (ChatMessage msg loc) = [("Message",show msg),("Location",show loc)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize ChatMessage where
  serialize (ChatMessage msg loc) = serialize msg <> serialize loc
  -- Chunk x, Chunk z, List of (chunk relative coords, Block Id (global palette))

data MultiBlockChange = MultiBlockChange (Int32,Int32) [((Word8,Word8,Word8),VarInt)]
instance Packet MultiBlockChange where
  type PacketSide MultiBlockChange = 'Client
  type PacketState MultiBlockChange = 'Playing
  packetName = "MultiBlockChange"
  packetId = 0x10
  packetPretty (MultiBlockChange _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize MultiBlockChange where
  serialize (MultiBlockChange _ _) = error "Unimplemented Serialization"
  -- Window Id, Transaction Id, Accepted

data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool
instance Packet ConfirmTransaction where
  type PacketSide ConfirmTransaction = 'Client
  type PacketState ConfirmTransaction = 'Playing
  packetName = "ConfirmTransaction"
  packetId = 0x11
  packetPretty (ConfirmTransaction _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize ConfirmTransaction where
  serialize (ConfirmTransaction wid transId acc) = serialize wid <> serialize transId <> serialize acc

data CloseWindow = CloseWindow WindowId
instance Packet CloseWindow where
  type PacketSide CloseWindow = 'Client
  type PacketState CloseWindow = 'Playing
  packetName = "CloseWindow"
  packetId = 0x12
  packetPretty (CloseWindow _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize CloseWindow where
  serialize (CloseWindow _) = error "Unimplemented Serialization"
  -- Window Id, Window Type (Enum), JSON chat string of Window Title, Num of Slots, optionally: EID of horse

data OpenWindow = OpenWindow WindowId String String Word8 (Maybe EntityId)
instance Packet OpenWindow where
  type PacketSide OpenWindow = 'Client
  type PacketState OpenWindow = 'Playing
  packetName = "OpenWindow"
  packetId = 0x13
  packetPretty (OpenWindow _ _ _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize OpenWindow where
  serialize (OpenWindow _ _ _ _ _) = error "Unimplemented Serialization"
  -- Window Id, List of <Slot>

data WindowItems = WindowItems WindowId [Slot]
instance Packet WindowItems where
  type PacketSide WindowItems = 'Client
  type PacketState WindowItems = 'Playing
  packetName = "WindowItems"
  packetId = 0x14
  packetPretty (WindowItems wid slots) = [("Window Id",show wid),("Slot Count",show (length slots))]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize WindowItems where
  serialize (WindowItems winId slots) = serialize winId <> serialize (genericLength slots :: Int16) <> BS.concat (map serialize slots)
  -- Window Id, Property (enum), Value (enum)

data WindowProperty = WindowProperty WindowId Short Short
instance Packet WindowProperty where
  type PacketSide WindowProperty = 'Client
  type PacketState WindowProperty = 'Playing
  packetName = "WindowProperty"
  packetId = 0x15
  packetPretty (WindowProperty _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize WindowProperty where
  serialize (WindowProperty _ _ _) = error "Unimplemented Serialization"
  -- Window Id, Slot num, <Slot>

data SetSlot = SetSlot WindowId Short Slot
instance Packet SetSlot where
  type PacketSide SetSlot = 'Client
  type PacketState SetSlot = 'Playing
  packetName = "SetSlot"
  packetId = 0x16
  packetPretty (SetSlot wid slotNum slot) = [("Window Id",show wid),("Slot Number",show slotNum),("Slot Data",show slot)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SetSlot where
  serialize (SetSlot wid slotNum slot) = serialize wid <> serialize slotNum <> serialize slot
  -- Item Id (applies to all instances), Cooldown Ticks

data SetCooldown = SetCooldown VarInt VarInt
instance Packet SetCooldown where
  type PacketSide SetCooldown = 'Client
  type PacketState SetCooldown = 'Playing
  packetName = "SetCooldown"
  packetId = 0x17
  packetPretty (SetCooldown _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SetCooldown where
  serialize (SetCooldown _ _) = error "Unimplemented Serialization"
  -- Plugin Channel, Data

data PluginMessage = PluginMessage String BS.ByteString
instance Packet PluginMessage where
  type PacketSide PluginMessage = 'Client
  type PacketState PluginMessage = 'Playing
  packetName = "PluginMessage"
  packetId = 0x18
  packetPretty (PluginMessage chan msg) = [("Plugin Channel",chan),("Message","0x" ++ (flip showHex "" =<< BS.unpack msg))]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize PluginMessage where
  serialize (PluginMessage str bs) = serialize str <> bs
  -- Sound Name (Enum), Sound Category (Enum), weird encoding for: x,y,z, Volume, Pitch

data NamedSoundEffect = NamedSoundEffect String VarInt (Int32,Int32,Int32) Float Float
instance Packet NamedSoundEffect where
  type PacketSide NamedSoundEffect = 'Client
  type PacketState NamedSoundEffect = 'Playing
  packetName = "NamedSoundEffect"
  packetId = 0x19
  packetPretty (NamedSoundEffect _ _ _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize NamedSoundEffect where
  serialize (NamedSoundEffect _ _ _ _ _) = error "Unimplemented Serialization"
  -- Reason (JSON chat string)

data DisconnectPlay = DisconnectPlay String
instance Packet DisconnectPlay where
  type PacketSide DisconnectPlay = 'Client
  type PacketState DisconnectPlay = 'Playing
  packetName = "DisconnectPlay"
  packetId = 0x1A
  packetPretty (DisconnectPlay _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize DisconnectPlay where
  serialize (DisconnectPlay _) = error "Unimplemented Serialization"
  -- EID, Status (Enum)

data EntityStatus = EntityStatus EntityId Word8
instance Packet EntityStatus where
  type PacketSide EntityStatus = 'Client
  type PacketState EntityStatus = 'Playing
  packetName = "EntityStatus"
  packetId = 0x1B
  packetPretty (EntityStatus _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize EntityStatus where
  serialize (EntityStatus _ _) = error "Unimplemented Serialization"
  -- x,y,z, radius, affected block offsets, velocity of pushed player

data Explosion = Explosion (Float,Float,Float) Float [BlockCoord] (Float,Float,Float)
instance Packet Explosion where
  type PacketSide Explosion = 'Client
  type PacketState Explosion = 'Playing
  packetName = "Explosion"
  packetId = 0x1C
  packetPretty (Explosion _ _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize Explosion where
  serialize (Explosion _ _ _ _) = error "Unimplemented Serialization"
  -- Chunk X, Chunk Z

data UnloadChunk = UnloadChunk (Int32,Int32)
instance Packet UnloadChunk where
  type PacketSide UnloadChunk = 'Client
  type PacketState UnloadChunk = 'Playing
  packetName = "UnloadChunk"
  packetId = 0x1D
  packetPretty (UnloadChunk _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize UnloadChunk where
  serialize (UnloadChunk _) = error "Unimplemented Serialization"
  -- Reason (Enum), Value (from Enum)

data ChangeGameState = ChangeGameState GameStateChange
instance Packet ChangeGameState where
  type PacketSide ChangeGameState = 'Client
  type PacketState ChangeGameState = 'Playing
  packetName = "ChangeGameState"
  packetId = 0x1E
  packetPretty (ChangeGameState _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize ChangeGameState where
  serialize (ChangeGameState InvalidBed) = BS.singleton 0x00 <> serialize (0 :: Float)
  serialize (ChangeGameState (Raining isStarting)) = BS.singleton (if isStarting then 0x01 else 0x02) <> serialize (0 :: Float)
  serialize (ChangeGameState (ChangeGamemode g)) = BS.singleton 0x03 <> serialize (case g of {Survival -> 0; Creative -> 1;} :: Float)
  serialize (ChangeGameState (ExitTheEnd showCredits)) = BS.singleton 0x04 <> if showCredits then serialize (1 :: Float) else serialize (0 :: Float)
  serialize (ChangeGameState DemoMessage) = BS.singleton 0x05 <> serialize (0 :: Float)
  serialize (ChangeGameState ArrowHitOtherPlayer) = BS.singleton 0x06 <> serialize (0 :: Float)
  serialize (ChangeGameState (FadeValue f)) = BS.singleton 0x07 <> serialize f
  serialize (ChangeGameState (FadeTime f)) = BS.singleton 0x08 <> serialize f
  serialize (ChangeGameState ElderGuardian) = BS.singleton 0x09 <> serialize (0 :: Float)
  -- Random Id <-- Prevents Timeout

data KeepAlive = KeepAlive KeepAliveId
instance Packet KeepAlive where
  type PacketSide KeepAlive = 'Client
  type PacketState KeepAlive = 'Playing
  packetName = "KeepAlive"
  packetId = 0x1F
  packetPretty (KeepAlive kid) = [("Keep Alive Id",show kid)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize KeepAlive where
  serialize (KeepAlive kid) = serialize kid
  -- Chunk X, Chunk Z, Full Chunk?, Bitmask of slices present, [Chunk Section], optional: 256 byte array of biome data, [Block entity NBT tag]

data ChunkData = ChunkData (Int32,Int32) Bool VarInt [ChunkSection] (Maybe BS.ByteString) [NBT]
instance Packet ChunkData where
  type PacketSide ChunkData = 'Client
  type PacketState ChunkData = 'Playing
  packetName = "ChunkData"
  packetId = 0x20
  packetPretty (ChunkData (cx,cz) guCont bitMask cs _mBio _nbt) = [("Column",show (cx,cz)),("Bit Mask",show bitMask)] ++ (if guCont then [("Full Chunk","")] else []) ++ [("Section Count",show (length cs))]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize ChunkData where
  serialize (ChunkData (cx,cz) guCont bitMask chunkSecs mBiomes blockEnts) = serialize cx <> serialize cz <> serialize guCont <> serialize bitMask <> withLength (BS.concat $ (map serialize chunkSecs) ++ maybeToList mBiomes) <> withListLength blockEnts
  -- Effect Id (Enum), block coord, extra data (from Enum), disable relative?

data Effect = Effect Int32 BlockCoord Int32 Bool
instance Packet Effect where
  type PacketSide Effect = 'Client
  type PacketState Effect = 'Playing
  packetName = "Effect"
  packetId = 0x21
  packetPretty (Effect _ _ _ _) = []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize Effect where
  serialize (Effect _ _ _ _) = error "Unimplemented Serialization"
  -- Particle
  -- | Particle
  -- EID, Gamemode (Enum), Dimension (Enum), Difficulty (Enum), Max Players (deprecated), Level Type, reduce debug info?

data JoinGame = JoinGame EntityId Gamemode Int32 Difficulty Word8 String Bool
instance Packet JoinGame where
  type PacketSide JoinGame = 'Client
  type PacketState JoinGame = 'Playing
  packetName = "JoinGame"
  packetId = 0x23
  packetPretty (JoinGame eid gm dim dif maxP lvl reduce) = [("Entity Id",show eid),("Gamemode", show gm),("Dimension",show dim),("Difficulty",show dif),("Max Players",show maxP),("Level Type",lvl)] ++ if reduce then [("Reduce Debug Info","")] else []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize JoinGame where
  -- For whatever reason, we need the eid as an Int32 here, not a VarInt
  serialize (JoinGame eid gamemode dim dif maxp leveltype reduce) = serialize (unVarInt (unEID eid)) <> serialize gamemode <> serialize dim <> serialize dif <> serialize maxp <> serialize leveltype <> serialize reduce
  -- Flags bitfield, fly speed, fov modifier

data PlayerAbilities = PlayerAbilities AbilityFlags Float Float
instance Packet PlayerAbilities where
  type PacketSide PlayerAbilities = 'Client
  type PacketState PlayerAbilities = 'Playing
  packetName = "PlayerAbilities"
  packetId = 0x2B
  packetPretty (PlayerAbilities (AbilityFlags i f af c) flySpeed fovMod) = u i "Invulnerable" ++ u f "Flying" ++ u af "Allow Flying" ++ u c "Creative" ++ [("Flying Speed",show flySpeed),("FOV Modifier",show fovMod)]
    where
      u b s = if b then [(s,"")] else []
  parsePacket = error "Can't parse clientbound packet"
instance Serialize PlayerAbilities where
  serialize (PlayerAbilities flag fly fov) = serialize flag <> serialize fly <> serialize fov
  -- x,y,z, yaw,pitch, relativity flags, TPconfirm Id

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
  parsePacket = error "Can't parse clientbound packet"
instance Serialize PlayerPositionAndLook where
  serialize (PlayerPositionAndLook (x,y,z) (yaw,pitch) relFlag tpId) = serialize x <> serialize y <> serialize z <> serialize yaw <> serialize pitch <> serialize relFlag <> serialize tpId
  -- Block pos of player spawn

data UpdateMetadata = UpdateMetadata EntityId [Maybe EntityMetadata]
instance Packet UpdateMetadata where
  type PacketSide UpdateMetadata = 'Client
  type PacketState UpdateMetadata = 'Playing
  packetName = "UpdateMetadata"
  packetId = 0x39
  packetPretty (UpdateMetadata _eid _mDats) = []
  -- TODO: subclass?
  parsePacket = error "Can't parse clientbound packet"
instance Serialize UpdateMetadata where
  serialize (UpdateMetadata eid mDats) = serialize eid <> serialize mDats
  -- Block pos of player spawn

data SpawnPosition = SpawnPosition BlockCoord
instance Packet SpawnPosition where
  type PacketSide SpawnPosition = 'Client
  type PacketState SpawnPosition = 'Playing
  packetName = "SpawnPosition"
  packetId = 0x43
  packetPretty (SpawnPosition block) = [("Spawn",show block)]
  parsePacket = error "Can't parse clientbound packet"
instance Serialize SpawnPosition where
  serialize (SpawnPosition pos) = serialize pos

-- All packets have their length and pktId annotated
-- serialize pkt = BS.append (serialize $ packetId @PlayerPositionAndLook) $ case pkt of

--instance Show Packet where
  --show pkt = formatPacket (packetName pkt) $ case pkt of
