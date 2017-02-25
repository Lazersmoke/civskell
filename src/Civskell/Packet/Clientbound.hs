{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Civskell.Packet.Clientbound where

import Crypto.Hash (hash,Digest,SHA1)
import Data.Bits
import Data.Int
import Data.List (genericLength)
import Data.Maybe
import Data.NBT
import Data.Semigroup
import Data.Word
import Numeric (showHex)
import qualified Data.ByteString as BS

import Civskell.Data.Types
import Civskell.Tech.Serialization

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
  | SpawnObject EntityId String Word8 (Double,Double,Double) (Word8,Word8) Int32 Short Short Short
  -- EID, x, y, z, count
  | SpawnExpOrb EntityId (Double,Double,Double) Short
  -- EID, Type (always 1 for thunderbolt), x, y, z
  | SpawnGlobalEntity EntityId Word8 (Double,Double,Double)
  -- EID, UUID, Type, x, y, z, yaw, pitch, head pitch, velx, vely, velz, Metadata
  | SpawnMob EntityId String VarInt (Double,Double,Double) Word8 Word8 Word8 Short Short Short -- Metadata NYI
  -- EID, UUID, Title, Location,
  | SpawnPainting EntityId String String {-Position NYI-} Word8
  -- EID, UUID, x, y, z, yaw, pitch, metadata
  | SpawnPlayer EntityId String (Double,Double,Double) Word8 Word8 -- Metadata NYI
  -- EID, Animation ID (from table)
  | Animation EntityId Word8
  -- List of all stats
  | Statistics [(String,VarInt)]
  -- EID, Block coord, stage (0-9)
  | BlockBreakAnimation EntityId BlockCoord Word8
  -- Block coord, action (from enum), NBT tag
  | UpdateBlockEntity BlockCoord Word8 BS.ByteString
  -- Block coord, Action Id (enum), Action Param, Block Type ; http://wiki.vg/Block_Actions
  | BlockAction BlockCoord (Word8,Word8) VarInt
  -- Block coord, Block ID (from global palette)
  | BlockChange BlockCoord BlockState
  -- UUID, Action (from enum)
  | BossBar String {- BossBarAction NYI -}
  -- Difficulty (0-3)
  | ServerDifficulty Difficulty
  -- List of matches for tab completion. Prefixed with length when sent
  | TabComplete [String]
  -- JSON chat string, place to appear in (0:chatbox,1:sys msg. chatbox,2:hotbar)
  | ChatMessage String Word8
  -- Chunk x, Chunk z, List of (chunk relative coords, Block Id (global palette))
  | MultiBlockChange (Int32,Int32) [((Word8,Word8,Word8),VarInt)]
  -- Window Id, Transaction Id, Accepted
  | ConfirmTransaction WindowId TransactionId Bool
  | CloseWindow WindowId
  -- Window Id, Window Type (Enum), JSON chat string of Window Title, Num of Slots, optionally: EID of horse
  | OpenWindow WindowId String String Word8 (Maybe EntityId)
  -- Window Id, List of <Slot>
  | WindowItems WindowId [Slot]
  -- Window Id, Property (enum), Value (enum)
  | WindowProperty WindowId Short Short
  -- Window Id, Slot num, <Slot>
  | SetSlot WindowId Short Slot
  -- Item Id (applies to all instances), Cooldown Ticks
  | SetCooldown VarInt VarInt
  -- Plugin Channel, Data
  | PluginMessage String BS.ByteString
  -- Sound Name (Enum), Sound Category (Enum), weird encoding for: x,y,z, Volume, Pitch
  | NamedSoundEffect String VarInt (Int32,Int32,Int32) Float Float
  -- Reason (JSON chat string)
  | DisconnectPlay String
  -- EID, Status (Enum)
  | EntityStatus EntityId Word8
  -- x,y,z, radius, affected block offsets, velocity of pushed player
  | Explosion (Float,Float,Float) Float [BlockCoord] (Float,Float,Float)
  -- Chunk X, Chunk Z
  | UnloadChunk (Int32,Int32)
  -- Reason (Enum), Value (from Enum)
  | ChangeGameState GameStateChange
  -- Random Id <-- Prevents Timeout
  | KeepAlive KeepAliveId
  -- Chunk X, Chunk Z, Full Chunk?, Bitmask of slices present, [Chunk Section], optional: 256 byte array of biome data, [Block entity NBT tag]
  | ChunkData (Int32,Int32) Bool VarInt [ChunkSection] (Maybe BS.ByteString) [NBT]
  -- Effect Id (Enum), block coord, extra data (from Enum), disable relative?
  | Effect Int32 BlockCoord Int32 Bool
  -- Particle
  -- | Particle
  -- EID, Gamemode (Enum), Dimension (Enum), Difficulty (Enum), Max Players (deprecated), Level Type, reduce debug info?
  | JoinGame EntityId Gamemode Int32 Difficulty Word8 String Bool
  -- Flags bitfield, fly speed, fov modifier
  | PlayerAbilities Word8 Float Float
  -- x,y,z, yaw,pitch, relativity flags, TPconfirm Id
  | PlayerPositionAndLook (Double,Double,Double) (Float,Float) Word8 TPConfirmId
  -- Block pos of player spawn
  | SpawnPosition BlockCoord

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
    (SpawnObject _ _ _ _ _ _ _ _ _) -> error "Unimplemented Serialization"
    (SpawnExpOrb _ _ _) -> error "Unimplemented Serialization"
    (SpawnGlobalEntity _ _ _) -> error "Unimplemented Serialization"
    (SpawnMob _ _ _ _ _ _ _ _ _ _) -> error "Unimplemented Serialization"
    (SpawnPainting _ _ _ {-NYI-} _) -> error "Unimplemented Serialization"
    (SpawnPlayer _ _ _ _ _ {-Metadata NYI-}) -> error "Unimplemented Serialization"
    (Animation eid anim) -> serialize eid <> serialize anim
    (Statistics _) -> error "Unimplemented Serialization"
    (BlockBreakAnimation _ _ _) -> error "Unimplemented Serialization"
    (UpdateBlockEntity _ _ _) -> error "Unimplemented Serialization"
    (BlockAction _ _ _) -> error "Unimplemented Serialization"
    (BlockChange block bs) -> serialize block <> serialize bs
    (BossBar _ {- BossBarAction NYI -}) -> error "Unimplemented Serialization"
    (ServerDifficulty dif) -> serialize dif
    (TabComplete _) -> error "Unimplemented Serialization"
    (ChatMessage msg loc) -> serialize msg <> serialize loc
    (MultiBlockChange _ _) -> error "Unimplemented Serialization"
    (ConfirmTransaction wid transId acc) -> serialize wid <> serialize transId <> serialize acc
    (CloseWindow _) -> error "Unimplemented Serialization"
    (OpenWindow _ _ _ _ _) -> error "Unimplemented Serialization"
    (WindowItems winId slots) -> serialize winId <> serialize (genericLength slots :: Int16) <> BS.concat (map serialize slots)
    (WindowProperty _ _ _) -> error "Unimplemented Serialization"
    (SetSlot wid slotNum slot) -> serialize wid <> serialize slotNum <> serialize slot
    (SetCooldown _ _) -> error "Unimplemented Serialization"
    (PluginMessage str bs) -> serialize str <> bs
    (NamedSoundEffect _ _ _ _ _) -> error "Unimplemented Serialization"
    (DisconnectPlay _) -> error "Unimplemented Serialization"
    (EntityStatus _ _) -> error "Unimplemented Serialization"
    (Explosion _ _ _ _) -> error "Unimplemented Serialization"
    (UnloadChunk _) -> error "Unimplemented Serialization"
    (ChangeGameState InvalidBed) -> BS.singleton 0x00 <> serialize (0 :: Float)
    (ChangeGameState (Raining isStarting)) -> BS.singleton (if isStarting then 0x01 else 0x02) <> serialize (0 :: Float)
    (ChangeGameState (ChangeGamemode g)) -> BS.singleton 0x03 <> serialize (case g of {Survival -> 0; Creative -> 1;} :: Float)
    (ChangeGameState (ExitTheEnd showCredits)) -> BS.singleton 0x04 <> if showCredits then serialize (1 :: Float) else serialize (0 :: Float)
    (ChangeGameState DemoMessage) -> BS.singleton 0x05 <> serialize (0 :: Float)
    (ChangeGameState ArrowHitOtherPlayer) -> BS.singleton 0x06 <> serialize (0 :: Float)
    (ChangeGameState (FadeValue f)) -> BS.singleton 0x07 <> serialize f
    (ChangeGameState (FadeTime f)) -> BS.singleton 0x08 <> serialize f
    (ChangeGameState ElderGuardian) -> BS.singleton 0x09 <> serialize (0 :: Float)
    (KeepAlive kid) -> serialize kid
    (ChunkData (cx,cz) guCont bitMask chunkSecs mBiomes blockEnts) -> serialize cx <> serialize cz <> serialize guCont <> serialize bitMask <> withLength (BS.concat $ (map serialize chunkSecs) ++ maybeToList mBiomes) <> withListLength blockEnts
    (Effect _ _ _ _) -> error "Unimplemented Serialization"
    (JoinGame eid gamemode dim dif maxp leveltype reduce) -> serialize eid <> serialize gamemode <> serialize dim <> serialize dif <> serialize maxp <> serialize leveltype <> serialize reduce
    (PlayerAbilities flag fly fov) -> serialize flag <> serialize fly <> serialize fov
    (PlayerPositionAndLook (x,y,z) (yaw,pitch) relFlag tpId) -> serialize x <> serialize y <> serialize z <> serialize yaw <> serialize pitch <> serialize relFlag <> serialize tpId
    (SpawnPosition pos) -> serialize pos

-- All packets have a packet ID
instance PacketId Packet where
  packetSide _ = Client
  packetName p = case p of
    -- Login
    (Disconnect _) -> "Disconnect"
    (EncryptionRequest _ _ _) -> "EncryptionRequest"
    (LoginSuccess _ _) -> "LoginSuccess"
    (SetCompression _) -> "SetCompression"
    -- Status
    (StatusResponse _) -> "StatusResponse"
    (StatusPong _) -> "StatusPong"
    -- Play
    (SpawnObject _ _ _ _ _ _ _ _ _) -> "SpawnObject"
    (SpawnExpOrb _ _ _) -> "SpawnExpOrb"
    (SpawnGlobalEntity _ _ _) -> "SpawnGlobalEntity"
    (SpawnMob _ _ _ _ _ _ _ _ _ _) -> "SpawnMob"
    (SpawnPainting _ _ _ {-NYI-} _) -> "SpawnPainting"
    (SpawnPlayer _ _ _ _ _ {-Metadata NYI-}) -> "SpawnPlayer"
    (Animation _ _) -> "Animation"
    (Statistics _) -> "Statistics"
    (BlockBreakAnimation _ _ _) -> "BlockBreakAnimation"
    (UpdateBlockEntity _ _ _) -> "UpdateBlockEntity"
    (BlockAction _ _ _) -> "BlockAction"
    (BlockChange _ _) -> "BlockChange"
    (BossBar _ {- BossBarAction NYI -}) -> "BossBar"
    (ServerDifficulty _) -> "ServerDifficulty"
    (TabComplete _) -> "TabComplete"
    (ChatMessage _ _) -> "ChatMessage"
    (MultiBlockChange _ _) -> "MultiBlockChange"
    (ConfirmTransaction _ _ _) -> "ConfirmTransaction"
    (CloseWindow _) -> "CloseWindow"
    (OpenWindow _ _ _ _ _) -> "OpenWindow"
    (WindowItems _ _) -> "WindowItems"
    (WindowProperty _ _ _) -> "WindowProperty"
    (SetSlot _ _ _) -> "SetSlot"
    (SetCooldown _ _) -> "SetCooldown"
    (PluginMessage _ _) -> "PluginMessage"
    (NamedSoundEffect _ _ _ _ _) -> "NamedSoundEffect"
    (DisconnectPlay _) -> "DisconnectPlay"
    (EntityStatus _ _) -> "EntityStatus"
    (Explosion _ _ _ _) -> "Explosion"
    (UnloadChunk _) -> "UnloadChunk"
    (ChangeGameState _) -> "ChangeGameState"
    (KeepAlive _) -> "KeepAlive"
    (ChunkData _ _ _ _ _ _) -> "ChunkData"
    (Effect _ _ _ _) -> "Effect"
    -- Paricle -> 0x22
    (JoinGame _ _ _ _ _ _ _) -> "JoinGame"
    (PlayerAbilities _ _ _) -> "PlayerAbilities"
    (PlayerPositionAndLook _ _ _ _) -> "PlayerPositionAndLook"
    (SpawnPosition _) -> "SpawnPosition"
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
    (SpawnPainting _ _ _ {-NYI-} _) -> 0x04
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
    (ChangeGameState _) -> 0x1E
    (KeepAlive _) -> 0x1F
    (ChunkData _ _ _ _ _ _) -> 0x20
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
    (SpawnPainting _ _ _ {-NYI-} _) -> Playing
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
    (ChangeGameState _) -> Playing
    (KeepAlive _) -> Playing
    (ChunkData _ _ _ _ _ _) -> Playing
    (Effect _ _ _ _) -> Playing
    (JoinGame _ _ _ _ _ _ _) -> Playing
    (PlayerAbilities _ _ _) -> Playing
    (PlayerPositionAndLook _ _ _ _) -> Playing
    (SpawnPosition _) -> Playing

instance Show Packet where
  show pkt = formatPacket (packetName pkt) $ case pkt of
    -- Login
    (Disconnect reason) -> [("Reason",reason)]
    (EncryptionRequest sId pubKey vt) -> [("Server Id",sId),("Public Key Hash",(take 7 $ show (hash pubKey :: Digest SHA1)) ++ "..."),("Verify Token","0x" ++ (flip showHex "" =<< BS.unpack vt))]
    (LoginSuccess uuid name) -> [("UUID",uuid),("Username",name)]
    (SetCompression thresh) -> [("Compresion Threshold",show thresh)]
    -- Status
    -- Beware: statusJSON includes a base64 encoded png, so it is very very long
    (StatusResponse _statusJSON) -> [("Status JSON","")]
    (StatusPong pongTok) -> [("Pong Token","0x" ++  showHex pongTok "")]
    -- Play
    (SpawnObject _ _ _ _ _ _ _ _ _) -> []
    (SpawnExpOrb _ _ _) -> []
    (SpawnGlobalEntity _ _ _) -> []
    (SpawnMob _ _ _ _ _ _ _ _ _ _) -> []
    (SpawnPainting _ _ _ {-NYI-} _) -> []
    (SpawnPlayer _ _ _ _ _ {-Metadata NYI-}) -> []
    (Animation eid anim) -> [("Entity Id",show eid),("Animation",show anim)]
    (Statistics _) -> []
    (BlockBreakAnimation _ _ _) -> []
    (UpdateBlockEntity _ _ _) -> []
    (BlockAction _ _ _) -> []
    (BlockChange bc bs) -> [("Block",show bc),("New State",show bs)]
    (BossBar _ {- BossBarAction NYI -}) -> []
    (ServerDifficulty dif) -> [("Difficulty",show dif)]
    (TabComplete _) -> []
    (ChatMessage msg loc) -> [("Message",show msg),("Location",show loc)]
    (MultiBlockChange _ _) -> []
    (ConfirmTransaction _ _ _) -> []
    (CloseWindow _) -> []
    (OpenWindow _ _ _ _ _) -> []
    (WindowItems wid slots) -> [("Window Id",show wid),("Slot Count",show (length slots))]
    (WindowProperty _ _ _) -> []
    (SetSlot wid slotNum slot) -> [("Window Id",show wid),("Slot Number",show slotNum),("Slot Data",show slot)]
    (SetCooldown _ _) -> []
    (PluginMessage chan msg) -> [("Plugin Channel",chan),("Message","0x" ++ (flip showHex "" =<< BS.unpack msg))]
    (NamedSoundEffect _ _ _ _ _) -> []
    (DisconnectPlay _) -> []
    (EntityStatus _ _) -> []
    (Explosion _ _ _ _) -> []
    (UnloadChunk _) -> []
    (ChangeGameState _) -> []
    (KeepAlive kid) -> [("Keep Alive Id",show kid)]
    (ChunkData (cx,cz) guCont bitMask cs _mBio _nbt) -> [("Column",show (cx,cz)),("Bit Mask",show bitMask)] ++ (if guCont then [("Full Chunk","")] else []) ++ [("Section Count",show (length cs))]
    (Effect _ _ _ _) -> []
    (JoinGame eid gm dim dif maxP lvl reduce) -> [("Entity Id",show eid),("Gamemode", show gm),("Dimension",show dim),("Difficulty",show dif),("Max Players",show maxP),("Level Type",lvl)] ++ if reduce then [("Reduce Debug Info","")] else []
    (PlayerAbilities flags flySpeed fovMod) -> [("Flags",show flags),("Fly Speed",show flySpeed),("FOV Modifier",show fovMod)]
    (PlayerPositionAndLook (x,y,z) (yaw,pitch) rel tid) ->
      [("X",r 1 (show x))
      ,("Y",r 2 (show y))
      ,("Z",r 3 (show z))
      ,("Yaw",r 4 (show yaw))
      ,("Pitch",r 5 (show pitch))
      ,("Teleport Id", show tid)
      ]
      where
        r b = if testBit rel b then ("~" ++) else id
    (SpawnPosition block) -> [("Spawn",show block)]
