{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Clientbound where

import Data.Int
import Data.Semigroup
import Data.Word
import Data.List (genericLength)
import qualified Data.ByteString as BS
import Data

data Packet
  -- Login
  -- Reason
  = Disconnect String
  -- Server ID, Pub Key, Verify Token
  | EncryptionRequest String BS.ByteString BS.ByteString
  -- UUID (with hyphens), Username
  | LoginSuccess String String
  -- Size threshold for compression
  | SetCompression VarInt
  -- Status
  -- JSON String (for now)
  | StatusResponse String
  -- Payload (unique number obtained from client)
  | StatusPong Int64
  -- Play
  -- EID, UUID, Type of Object, x, y, z, pitch, yaw, Object Data, velx, vely, velz
  | SpawnObject VarInt String Word8 (Position Double) (Word8,Word8) Int32 Short Short Short
  -- EID, x, y, z, count
  | SpawnExpOrb VarInt (Position Double) Short
  -- EID, Type (always 1 for thunderbolt), x, y, z
  | SpawnGlobalEntity VarInt Word8 (Position Double)
  -- EID, UUID, Type, x, y, z, yaw, pitch, head pitch, velx, vely, velz, Metadata
  | SpawnMob VarInt String VarInt (Position Double) Word8 Word8 Word8 Short Short Short -- Metadata NYI
  -- EID, UUID, Title, Location, 
  | SpawnPainting VarInt String String {-Position NYI-} Word8
  -- EID, UUID, x, y, z, yaw, pitch, metadata
  | SpawnPlayer VarInt String (Position Double) Word8 Word8 -- Metadata NYI
  -- EID, Animation ID (from table)
  | Animation VarInt Word8
  -- List of all stats
  | Statistics [(String,VarInt)]
  -- EID, Block coord, stage (0-9)
  | BlockBreakAnimation VarInt (Position Int) Word8
  -- Block coord, action (from enum), NBT tag
  | UpdateBlockEntity (Position Int) Word8 BS.ByteString
  -- Block coord, Action Id (enum), Action Param, Block Type ; http://wiki.vg/Block_Actions
  | BlockAction (Position Int) (Word8,Word8) VarInt
  -- Block coord, Block ID (from global palette)
  | BlockChange (Position Int) VarInt
  -- UUID, Action (from enum)
  | BossBar String {- BossBarAction NYI -}
  -- Difficulty (0-3)
  | ServerDifficulty Word8
  -- List of matches for tab completion. Prefixed with length when sent
  | TabComplete [String]
  -- JSON chat string, (Position Int) to appear in (0:chatbox,1:sys msg. chatbox,2:hotbar)
  | ChatMessage String Word8
  -- Chunk x, Chunk z, List of (chunk relative coords, Block Id (global palette))
  | MultiBlockChange (Int32,Int32) [((Word8,Word8,Word8),VarInt)]
  -- Window Id, Action Number (Enum?), Accepted
  | ConfirmTransaction Word8 Short Bool
  -- Window Id
  | CloseWindow Word8
  -- Window Id, Window Type (Enum), JSON chat string of Window Title, Num of Slots, optionally: EID of horse
  | OpenWindow Word8 String String Word8 (Maybe Int32)
  -- Window Id, List of <Slot>
  | WindowItems Word8 [Slot]
  -- Window Id, Property (enum), Value (enum)
  | WindowProperty Word8 Short Short
  -- Window Id, Slot num, <Slot>
  | SetSlot Word8 Short Slot
  -- Item Id (applies to all instances), Cooldown Ticks
  | SetCooldown VarInt VarInt
  -- Plugin Channel, Data
  | PluginMessage String BS.ByteString
  -- Sound Name (Enum), Sound Category (Enum), weird encoding for: x,y,z, Volume, Pitch
  | NamedSoundEffect String VarInt (Int32,Int32,Int32) Float Float
  -- Reason (JSON chat string)
  | DisconnectPlay String
  -- EID, Status (Enum)
  | EntityStatus Int32 Word8
  -- x,y,z, radius, affected block offsets, velocity of pushed player
  | Explosion (Float,Float,Float) Float [(Word8,Word8,Word8)] (Float,Float,Float)
  -- Chunk X, Chunk Z
  | UnloadChunk (Int32,Int32)
  -- Reason (Enum), Value (from Enum)
  | ChangeGameState Word8 Float
  -- Random Id <-- Prevents Timeout
  | KeepAlive VarInt
  -- Chunk X, Chunk Z, Full Chunk?, Bitmask of slices present, [Chunk Section], optional: 256 byte array of biome data, [Block entity NBT tag]
  | ChunkData (Int32,Int32) Bool VarInt {-[ChunkSection] NYI-} (Maybe BS.ByteString) {-[NBT Tag] NYI-}
  -- Effect Id (Enum), block coord, extra data (from Enum), disable relative?
  | Effect Int32 (Position Int) Int32 Bool
  -- Particle
  -- | Particle
  -- EID, Gamemode (Enum), Dimension (Enum), Difficulty (Enum), Max Players (deprecated), Level Type, reduce debug info?
  | JoinGame Int32 Word8 Int32 Word8 Word8 String Bool
  -- Flags bitfield, fly speed, fov modifier
  | PlayerAbilities Word8 Float Float
  -- x,y,z, yaw,pitch, relativity flags, TPconfirm Id
  | PlayerPositionAndLook (Position Double) (Float,Float) Word8 VarInt
  -- Block pos of player spawn
  | SpawnPosition (Position Int) deriving Show

-- All packets have their length and pktId annotated
instance Serialize Packet where
  serialize pkt = BS.append (serialize $ packetId pkt) $ case pkt of
    -- Login
    (Disconnect reason) -> serialize reason
    (EncryptionRequest sId p vt) -> serialize sId <> withLength p <> withLength vt
    (LoginSuccess uuid name) -> serialize uuid <> serialize name
    (SetCompression thresh) -> serialize thresh
    -- Status
    (StatusResponse s) -> serialize s
    (StatusPong l) -> serialize l
    -- Play 
    (SpawnObject _ _ _ _ _ _ _ _ _) -> BS.singleton 0x00
    (SpawnExpOrb _ _ _) -> BS.singleton 0x00
    (SpawnGlobalEntity _ _ _) -> BS.singleton 0x00
    (SpawnMob _ _ _ _ _ _ _ _ _ _) -> BS.singleton 0x00
    (SpawnPainting _ _ _ {-Position NYI-} _) -> BS.singleton 0x00
    (SpawnPlayer _ _ _ _ _ {-Metadata NYI-}) -> BS.singleton 0x00
    (Animation eid anim) -> serialize eid <> serialize anim
    (Statistics _) -> BS.singleton 0x00
    (BlockBreakAnimation _ _ _) -> BS.singleton 0x00
    (UpdateBlockEntity _ _ _) -> BS.singleton 0x00
    (BlockAction _ _ _) -> BS.singleton 0x00
    (BlockChange _ _) -> BS.singleton 0x00
    (BossBar _ {- BossBarAction NYI -}) -> BS.singleton 0x00
    (ServerDifficulty dif) -> BS.singleton dif
    (TabComplete _) -> BS.singleton 0x00
    (ChatMessage _ _) -> BS.singleton 0x00
    (MultiBlockChange _ _) -> BS.singleton 0x00
    (ConfirmTransaction _ _ _) -> BS.singleton 0x00
    (CloseWindow _) -> BS.singleton 0x00
    (OpenWindow _ _ _ _ _) -> BS.singleton 0x00
    (WindowItems winId slots) -> serialize winId <> serialize (genericLength slots :: Int16) <> BS.concat (map serialize slots)
    (WindowProperty _ _ _) -> BS.singleton 0x00
    (SetSlot _ _ _) -> BS.singleton 0x00
    (SetCooldown _ _) -> BS.singleton 0x00
    (PluginMessage str bs) -> serialize str <> bs
    (NamedSoundEffect _ _ _ _ _) -> BS.singleton 0x00
    (DisconnectPlay _) -> BS.singleton 0x00
    (EntityStatus _ _) -> BS.singleton 0x00
    (Explosion _ _ _ _) -> BS.singleton 0x00
    (UnloadChunk _) -> BS.singleton 0x00
    (ChangeGameState _ _) -> BS.singleton 0x00
    (KeepAlive kid) -> serialize kid
    (ChunkData _ _ _ {-[ChunkSection] NYI-} _ {-[NBT Tag] NYI-}) -> BS.singleton 0x00
    (Effect _ _ _ _) -> BS.singleton 0x00
    (JoinGame eid gamemode dim dif maxp leveltype reduce) -> serialize eid <> serialize gamemode <> serialize dim <> serialize dif <> serialize maxp <> serialize leveltype <> serialize reduce
    (PlayerAbilities flag fly fov) -> serialize flag <> serialize fly <> serialize fov
    (PlayerPositionAndLook (Position x y z) (yaw,pitch) relFlag tpId) -> serialize x <> serialize y <> serialize z <> serialize yaw <> serialize pitch <> serialize relFlag <> serialize tpId
    (SpawnPosition pos) -> serialize pos

-- All packets have a packet ID
instance PacketId Packet where
  packetSide _ = Client
  packetId p = case p of
    -- Login
    (Disconnect _) -> 0x00
    (EncryptionRequest _ _ _) -> 0x01
    (LoginSuccess _ _) -> 0x02
    (SetCompression _) -> 0x03
    -- Status
    (StatusResponse _) -> 0x00
    (StatusPong _) -> 0x01
    -- Play
    (SpawnObject _ _ _ _ _ _ _ _ _) -> 0x00
    (SpawnExpOrb _ _ _) -> 0x01
    (SpawnGlobalEntity _ _ _) -> 0x02
    (SpawnMob _ _ _ _ _ _ _ _ _ _) -> 0x03
    (SpawnPainting _ _ _ {-Position NYI-} _) -> 0x04
    (SpawnPlayer _ _ _ _ _ {-Metadata NYI-}) -> 0x05
    (Animation _ _) -> 0x06
    (Statistics _) -> 0x07
    (BlockBreakAnimation _ _ _) -> 0x08
    (UpdateBlockEntity _ _ _) -> 0x09
    (BlockAction _ _ _) -> 0x0A
    (BlockChange _ _) -> 0x0B
    (BossBar _ {- BossBarAction NYI -}) -> 0x0C
    (ServerDifficulty _) -> 0x0D
    (TabComplete _) -> 0x0E
    (ChatMessage _ _) -> 0x0F
    (MultiBlockChange _ _) -> 0x10
    (ConfirmTransaction _ _ _) -> 0x11
    (CloseWindow _) -> 0x12
    (OpenWindow _ _ _ _ _) -> 0x13
    (WindowItems _ _) -> 0x14
    (WindowProperty _ _ _) -> 0x15
    (SetSlot _ _ _) -> 0x16
    (SetCooldown _ _) -> 0x17
    (PluginMessage _ _) -> 0x18
    (NamedSoundEffect _ _ _ _ _) -> 0x19
    (DisconnectPlay _) -> 0x1A
    (EntityStatus _ _) -> 0x1B
    (Explosion _ _ _ _) -> 0x1C
    (UnloadChunk _) -> 0x1D
    (ChangeGameState _ _) -> 0x1E
    (KeepAlive _) -> 0x01F
    (ChunkData _ _ _ {-[ChunkSection] NYI-} _ {-[NBT Tag] NYI-}) -> 0x20
    (Effect _ _ _ _) -> 0x21
    -- Paricle -> 0x22
    (JoinGame _ _ _ _ _ _ _) -> 0x23
    (PlayerAbilities _ _ _) -> 0x2B
    (PlayerPositionAndLook _ _ _ _) -> 0x2E
    (SpawnPosition _) -> 0x43
  packetState p = case p of
    -- Login
    (Disconnect _) -> LoggingIn
    (EncryptionRequest _ _ _) -> LoggingIn
    (LoginSuccess _ _) -> LoggingIn
    (SetCompression _) -> LoggingIn
    -- Status
    (StatusResponse _) -> Status
    (StatusPong _) -> Status
    -- Play
    (SpawnObject _ _ _ _ _ _ _ _ _) -> Playing
    (SpawnExpOrb _ _ _) -> Playing
    (SpawnGlobalEntity _ _ _) -> Playing
    (SpawnMob _ _ _ _ _ _ _ _ _ _) -> Playing
    (SpawnPainting _ _ _ {-Position NYI-} _) -> Playing
    (SpawnPlayer _ _ _ _ _ {-Metadata NYI-}) -> Playing
    (Animation _ _) -> Playing
    (Statistics _) -> Playing
    (BlockBreakAnimation _ _ _) -> Playing
    (UpdateBlockEntity _ _ _) -> Playing
    (BlockAction _ _ _) -> Playing
    (BlockChange _ _) -> Playing
    (BossBar _ {- BossBarAction NYI -}) -> Playing
    (ServerDifficulty _) -> Playing
    (TabComplete _) -> Playing
    (ChatMessage _ _) -> Playing
    (MultiBlockChange _ _) -> Playing
    (ConfirmTransaction _ _ _) -> Playing
    (CloseWindow _) -> Playing
    (OpenWindow _ _ _ _ _) -> Playing
    (WindowItems _ _) -> Playing
    (WindowProperty _ _ _) -> Playing
    (SetSlot _ _ _) -> Playing
    (SetCooldown _ _) -> Playing
    (PluginMessage _ _) -> Playing
    (NamedSoundEffect _ _ _ _ _) -> Playing
    (DisconnectPlay _) -> Playing
    (EntityStatus _ _) -> Playing
    (Explosion _ _ _ _) -> Playing
    (UnloadChunk _) -> Playing
    (ChangeGameState _ _) -> Playing
    (KeepAlive _) -> Playing
    (ChunkData _ _ _ {-[ChunkSection] NYI-} _ {-[NBT Tag] NYI-}) -> Playing
    (Effect _ _ _ _) -> Playing
    (JoinGame _ _ _ _ _ _ _) -> Playing
    (PlayerAbilities _ _ _) -> Playing
    (PlayerPositionAndLook _ _ _ _) -> Playing
    (SpawnPosition _) -> Playing
    
