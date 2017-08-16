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
import Civskell.Data.Protocol
import Civskell.Item
import Civskell.Window

defaultDescriptor :: Serial p => ServerState -> T.Text -> (p -> [(T.Text,T.Text)]) -> VarInt -> OutboundPacketDescriptor p
defaultDescriptor ss name pret pktId = PacketDescriptor 
  {packetState = ss
  ,packetName = name 
  ,packetPretty = pret
  ,packetHandler = defaultSerializer pktId
  }

data LegacyHandshakePong = LegacyHandshakePong
legacyHandshakePong :: VarInt -> OutboundPacketDescriptor LegacyHandshakePong
legacyHandshakePong = defaultDescriptor Handshaking "LegacyHandshakePong" $ const []

instance Serial LegacyHandshakePong where
  serialize LegacyHandshakePong = putByteString legacyHandshakePongConstant
  deserialize = getByteString (BS.length legacyHandshakePongConstant) >>= \b -> if b /= legacyHandshakePongConstant then fail "Failed: deserialize @LegacyHandshakePong" else pure LegacyHandshakePong

data Disconnect = Disconnect ProtocolString deriving (Generic,Serial)
disconnect :: VarInt -> OutboundPacketDescriptor Disconnect
disconnect = defaultDescriptor LoggingIn "Disconnect" $ \(Disconnect reason) -> [("Reason",showText . unProtocolString $ reason)]

-- Server ID, Pub Key, Verify Token
data EncryptionRequest = EncryptionRequest ProtocolString LengthAnnotatedByteString LengthAnnotatedByteString deriving (Generic,Serial)
encryptionRequest :: VarInt -> OutboundPacketDescriptor EncryptionRequest
encryptionRequest = defaultDescriptor LoggingIn "EncryptionRequest" $ \(EncryptionRequest (ProtocolString sId) (LengthAnnotatedByteString pubKey) (LengthAnnotatedByteString vt)) -> [("Server Id",T.pack sId),("Public Key Hash",(T.take 7 $ showText (hash pubKey :: Digest SHA1)) <> "..."),("Verify Token",T.pack $ "0x" ++ (flip showHex "" =<< BS.unpack vt))]

-- UUID (with hyphens), Username
data LoginSuccess = LoginSuccess ProtocolString ProtocolString deriving (Generic,Serial)
loginSuccess :: VarInt -> OutboundPacketDescriptor LoginSuccess
loginSuccess = defaultDescriptor LoggingIn "LoginSuccess" $ \(LoginSuccess (ProtocolString uuid) (ProtocolString name)) -> [("UUID",T.pack uuid),("Username",T.pack name)]

-- Size threshold for compression
data SetCompression = SetCompression VarInt deriving (Generic,Serial)
setCompression :: VarInt -> OutboundPacketDescriptor SetCompression
setCompression = defaultDescriptor LoggingIn "SetCompression" $ \(SetCompression thresh) -> [("Compression Threshold",showText thresh)]

-- JSON String (for now)
data StatusResponse = StatusResponse ProtocolString deriving (Generic,Serial)
statusResponse :: VarInt -> OutboundPacketDescriptor StatusResponse
statusResponse = defaultDescriptor Status "StatusResponse" $ \(StatusResponse (ProtocolString statusJSON)) -> [("Status JSON",T.pack $ take 200 statusJSON)]
  -- Beware: statusJSON includes a base64 encoded png, so it is very very long

-- Payload (unique number obtained from client)
data StatusPong = StatusPong Int64 deriving (Generic,Serial)
statusPong :: VarInt -> OutboundPacketDescriptor StatusPong
statusPong = defaultDescriptor Status "StatusPong" $ \(StatusPong pongTok) -> [("Pong Token",T.pack $ "0x" ++ showHex pongTok "")]

-- EID, UUID, Type of Object, x, y, z, pitch, yaw, Object Data, velx, vely, velz
data SpawnObject = SpawnObject EntityId UUID (Some Object)
spawnObject :: VarInt -> OutboundPacketDescriptor SpawnObject
spawnObject = defaultDescriptor Playing "SpawnObject" $ pp
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
  deserialize = error "Undefined: deserialize @SpawnObject"

-- EID, x, y, z, count
data SpawnExpOrb = SpawnExpOrb EntityId (Double,Double,Double) Short deriving (Generic,Serial)
spawnExpOrb :: VarInt -> OutboundPacketDescriptor SpawnExpOrb
spawnExpOrb = defaultDescriptor Playing "SpawnExpOrb" $ \(SpawnExpOrb _ _ _) -> []

-- EID, Type (always 1 for thunderbolt), x, y, z
data SpawnGlobalEntity = SpawnGlobalEntity EntityId Word8 (Double,Double,Double) deriving (Generic,Serial)
spawnGlobalEntity :: VarInt -> OutboundPacketDescriptor SpawnGlobalEntity
spawnGlobalEntity = defaultDescriptor Playing "SpawnGlobalEntity" $ \(SpawnGlobalEntity _ _ _) -> []

-- EID, UUID, Type, x, y, z, yaw, pitch, head pitch, velx, vely, velz, Metadata
--data SpawnMob = SpawnMob EntityId UUID (Some Entity) Word8 EntityPropertySet deriving (Generic,Serial)
data SpawnMob = SpawnMob EntityId UUID VarInt (Double,Double,Double) (Word8,Word8,Word8) (Short,Short,Short) EntityPropertySet deriving (Generic,Serial)
spawnMob :: VarInt -> OutboundPacketDescriptor SpawnMob
spawnMob = defaultDescriptor Playing "SpawnMob" $ \(SpawnMob eid uuid mobType pos look vel _props) -> [("Entity Id",showText eid),("UUID",showText uuid),("Type",showText mobType),("Position",showText pos),("Looking",showText look),("Velocity",showText vel)]

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
spawnPainting :: VarInt -> OutboundPacketDescriptor SpawnPainting
spawnPainting = defaultDescriptor Playing "SpawnPainting" $ \(SpawnPainting _ _ _ _ _) -> []

-- EID, UUID, x, y, z, yaw, pitch, metadata
data SpawnPlayer = SpawnPlayer EntityId UUID (Double,Double,Double) Word8 Word8 EntityPropertySet deriving (Generic,Serial)
spawnPlayer :: VarInt -> OutboundPacketDescriptor SpawnPlayer
spawnPlayer = defaultDescriptor Playing "SpawnPlayer" $ \(SpawnPlayer _ _ _ _ _ _) -> []

-- EID, Animation ID (from table)
data Animation = Animation EntityId Word8 deriving (Generic,Serial)
animation :: VarInt -> OutboundPacketDescriptor Animation
animation = defaultDescriptor Playing "Animation" $ \(Animation eid anim) -> [("Entity Id",showText eid),("Animation",showText anim)]

-- List of all stats
data Statistics = Statistics (ProtocolList VarInt (ProtocolString,VarInt)) deriving (Generic,Serial)
statistics :: VarInt -> OutboundPacketDescriptor Statistics
statistics = defaultDescriptor Playing "Statistics" $ \(Statistics _) -> []

-- EID, Block coord, stage (0-9)
data BlockBreakAnimation = BlockBreakAnimation EntityId BlockCoord Word8 deriving (Generic,Serial)
blockBreakAnimation :: VarInt -> OutboundPacketDescriptor BlockBreakAnimation
blockBreakAnimation = defaultDescriptor Playing "BlockBreakAnimation" $ \(BlockBreakAnimation _ _ _) -> []

data UpdateBlockEntity = UpdateBlockEntity BlockCoord Word8 ProtocolNBT deriving (Generic,Serial)
updateBlockEntity :: VarInt -> OutboundPacketDescriptor UpdateBlockEntity
updateBlockEntity = defaultDescriptor Playing "UpdateBlockEntity" $ \(UpdateBlockEntity _ _ _) -> []

-- Block coord, Action Id (enum), Action Param, Block Type ; http://wiki.vg/Block_Actions
data BlockAction = BlockAction BlockCoord (Word8,Word8) VarInt deriving (Generic,Serial)
blockAction :: VarInt -> OutboundPacketDescriptor BlockAction
blockAction = defaultDescriptor Playing "BlockAction" $ \(BlockAction _ _ _) -> []

-- Block coord, Block ID (from global palette)
data BlockChange = BlockChange BlockCoord (Some Block)
blockChange :: VarInt -> OutboundPacketDescriptor BlockChange
blockChange = defaultDescriptor Playing "BlockChange" $ \(BlockChange bc bs) -> [("Block",showText bc),("New State",T.pack $ ambiguously (showBlock . runIdentity) bs)]
instance Serial BlockChange where
  serialize (BlockChange bc bs) = serialize bc *> ambiguously (serializeBlock . runIdentity) bs
  deserialize = error "Unimplemented: deserialize @BlockChange" --BlockChange <$> deserialize @BlockCoord <*> (ambiguate <$> deser

-- UUID, Action (from enum)
data BossBar = BossBar String {- BossBarAction NYI -}
bossBar :: VarInt -> OutboundPacketDescriptor BossBar
bossBar = defaultDescriptor Playing "BossBar" $ \(BossBar _ {- BossBarAction NYI -}) -> []
instance Serial BossBar where
  serialize (BossBar _ {- BossBarAction NYI -}) = error "Unimplemented serialize @BossBar"
  deserialize = error "Unimplemented: deserialize @BossBar"

-- Difficulty (0-3)
data ServerDifficulty = ServerDifficulty Difficulty deriving (Generic,Serial)
serverDifficulty :: VarInt -> OutboundPacketDescriptor ServerDifficulty
serverDifficulty = defaultDescriptor Playing "ServerDifficulty" $ \(ServerDifficulty dif) -> [("Difficulty",showText dif)]

-- List of matches for tab completion. Prefixed with length when sent
data TabComplete = TabComplete (ProtocolList VarInt ProtocolString) deriving (Generic,Serial)
tabComplete :: VarInt -> OutboundPacketDescriptor TabComplete
tabComplete = defaultDescriptor Playing "TabComplete" $ \(TabComplete _) -> []

-- JSON chat string, place to appear in (0:chatbox,1:sys msg. chatbox,2:hotbar)
data ChatMessage = ChatMessage ProtocolString Word8 deriving (Generic,Serial)
chatMessage :: VarInt -> OutboundPacketDescriptor ChatMessage
chatMessage = defaultDescriptor Playing "ChatMessage" $ \(ChatMessage msg loc) -> [("Message",showText . unProtocolString $ msg),("Location",showText loc)]

-- Chunk x, Chunk z, List of (chunk relative coords in special format, Block Id (global palette))
data MultiBlockChange = MultiBlockChange (Int32,Int32) (ProtocolList VarInt ((Word8,Word8),VarInt)) deriving (Generic,Serial)
multiBlockChange :: VarInt -> OutboundPacketDescriptor MultiBlockChange
multiBlockChange = defaultDescriptor Playing "MultiBlockChange" $ \(MultiBlockChange _ _) -> []

-- Window Id, Transaction Id, Accepted
data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool deriving (Generic,Serial)
confirmTransaction :: VarInt -> OutboundPacketDescriptor ConfirmTransaction
confirmTransaction = defaultDescriptor Playing "ConfirmTransaction" $ \(ConfirmTransaction _ _ _) -> []

data CloseWindow = CloseWindow WindowId deriving (Generic,Serial)
closeWindow :: VarInt -> OutboundPacketDescriptor CloseWindow
closeWindow = defaultDescriptor Playing "CloseWindow" $ \(CloseWindow _) -> []

-- Window Id, Window Type (Enum), JSON chat string of Window Title, Num of Slots, optionally: EID of horse
data OpenWindow = OpenWindow WindowId (Some Window) ProtocolString (Maybe EntityId)
openWindow :: VarInt -> OutboundPacketDescriptor OpenWindow
openWindow = defaultDescriptor Playing "OpenWindow" $ \(OpenWindow wid (SuchThat (Identity (_ :: winT))) title horseEid) -> [("Window Id",showText wid),("Window Type",windowName @winT),("Window Title",showText . unProtocolString $ title)] ++ (case horseEid of {Just eid -> [("Horse EID",showText eid)];Nothing -> []})
instance Serial OpenWindow where
  serialize (OpenWindow wid (SuchThat (Identity (_ :: winT))) title horseEid) = serialize wid *> serialize (windowIdentifier @winT) *> serialize title *> serialize ((unsafeCoerce :: Short -> Word8) $ slotCount @winT) *> (case horseEid of {Just eid -> serialize eid;Nothing -> pure ()})
  deserialize = error "Unimplemented: deserialize @OpenWindow"

-- Window Id, Slots
data WindowItems = WindowItems WindowId (ProtocolList Short Slot) deriving (Generic,Serial)
windowItems :: VarInt -> OutboundPacketDescriptor WindowItems
windowItems = defaultDescriptor Playing "WindowItems" $ \(WindowItems wid _slots) -> [("Window Id",showText wid)]
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
windowProperty :: VarInt -> OutboundPacketDescriptor WindowProperty
windowProperty = defaultDescriptor Playing "WindowProperty" $ \(WindowProperty _ _ _) -> []

-- Window Id, Slot num, <Slot>
data SetSlot = SetSlot WindowId Short Slot deriving (Generic,Serial)
setSlot :: VarInt -> OutboundPacketDescriptor SetSlot
setSlot = defaultDescriptor Playing "SetSlot" $ \(SetSlot wid slotNum slotData) -> [("Window Id",showText wid),("Slot Number",showText slotNum),("Slot Data",showText slotData)]

-- Item Id (applies to all instances), Cooldown Ticks
data SetCooldown = SetCooldown VarInt VarInt deriving (Generic,Serial)
setCooldown :: VarInt -> OutboundPacketDescriptor SetCooldown
setCooldown = defaultDescriptor Playing "SetCooldown" $ \(SetCooldown _ _) -> []

-- Plugin Channel, Data
data PluginMessage = PluginMessage ProtocolString BS.ByteString
--TODO: This hex part is very bad at indentation
pluginMessage :: VarInt -> OutboundPacketDescriptor PluginMessage
pluginMessage = defaultDescriptor Playing "PluginMessage" $ \(PluginMessage chan msg) -> [("Plugin Channel",T.pack $ unProtocolString chan),("Message",T.pack $ "0x" ++ (flip showHex "" =<< BS.unpack msg))]

-- Dont use generics because it would do the sensible thing and prepend the length.
-- Mojang doesnt do this, so we dont either
instance Serial PluginMessage where
  serialize (PluginMessage str bs) = serialize str *> putByteString bs
  deserialize = error "Cant generically deserialize a PluginMessage"

-- Sound Name (Enum), Sound Category (Enum), weird encoding for: x,y,z, Volume, Pitch
data NamedSoundEffect = NamedSoundEffect ProtocolString VarInt (Int32,Int32,Int32) Float Float deriving (Generic,Serial)
namedSoundEffect :: VarInt -> OutboundPacketDescriptor NamedSoundEffect
namedSoundEffect = defaultDescriptor Playing "NamedSoundEffect" $ \(NamedSoundEffect _ _ _ _ _) -> []

-- Reason (JSON chat string)
data DisconnectPlay = DisconnectPlay ProtocolString deriving (Generic,Serial)
disconnectPlay :: VarInt -> OutboundPacketDescriptor DisconnectPlay
disconnectPlay = defaultDescriptor Playing "DisconnectPlay" $ \(DisconnectPlay _) -> []

-- EID, Status (Enum)
data EntityStatus = EntityStatus EntityId Word8 deriving (Generic,Serial)
entityStatus :: VarInt -> OutboundPacketDescriptor EntityStatus
entityStatus = defaultDescriptor Playing "EntityStatus" $ \(EntityStatus _ _) -> []

-- x,y,z, radius, affected block offsets, velocity of pushed player
data Explosion = Explosion (Float,Float,Float) Float [(Word8,Word8,Word8)] (Float,Float,Float) deriving (Generic,Serial)
explosion :: VarInt -> OutboundPacketDescriptor Explosion
explosion = defaultDescriptor Playing "Explosion" $ \(Explosion _ _ _ _) -> []

-- Chunk X, Chunk Z
data UnloadChunk = UnloadChunk (Int32,Int32) deriving (Generic,Serial)
unloadChunk :: VarInt -> OutboundPacketDescriptor UnloadChunk
unloadChunk = defaultDescriptor Playing "UnloadChunk" $ \(UnloadChunk _) -> []

-- Reason (Enum), Value (from Enum)
data ChangeGameState = ChangeGameState GameStateChange
changeGameState :: VarInt -> OutboundPacketDescriptor ChangeGameState
changeGameState = defaultDescriptor Playing "ChangeGameState" $ \(ChangeGameState _) -> []
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
keepAlive :: VarInt -> OutboundPacketDescriptor KeepAlive
keepAlive = defaultDescriptor Playing "KeepAlive" $ \(KeepAlive kid) -> [("Keep Alive Id",showText kid)]

-- Chunk X, Chunk Z, Full Chunk?, Bitmask of slices present, [Chunk Section], optional: 256 byte array of biome data, [Block entity NBT tag]
data ChunkData = ChunkData (Int32,Int32) Bool VarInt [ChunkSection] (Maybe BS.ByteString) (ProtocolList VarInt ProtocolNBT)
chunkData :: VarInt -> OutboundPacketDescriptor ChunkData
chunkData = defaultDescriptor Playing "ChunkData" $ \(ChunkData (cx,cz) guCont bitMask cs _mBio _nbt) -> [("Column",showText (cx,cz)),("Bit Mask",showText bitMask)] ++ (if guCont then [("Full Chunk","")] else []) ++ [("Section Count",showText (length cs))]
instance Serial ChunkData where
  serialize (ChunkData (cx,cz) guCont bitMask chunkSecs mBiomes blockEnts) = serialize cx *> serialize cz *> serialize guCont *> serialize bitMask *> withLength (runPutS $ (traverse serialize chunkSecs) *> maybe (pure ()) putByteString mBiomes) *> serialize @(ProtocolList VarInt ProtocolNBT) blockEnts
  deserialize = error "Undefined: deserialize @ChunkData"

-- Effect Id (Enum), block coord, extra data (from Enum), disable relative?
data Effect = Effect Int32 BlockCoord Int32 Bool deriving (Generic,Serial)
effect :: VarInt -> OutboundPacketDescriptor Effect
effect = defaultDescriptor Playing "Effect" $ \(Effect _ _ _ _) -> []

data JoinGame = JoinGame EntityId Gamemode Dimension Difficulty Word8 ProtocolString Bool
joinGame :: VarInt -> OutboundPacketDescriptor JoinGame
joinGame = defaultDescriptor Playing "JoinGame" $ \(JoinGame eid gm dim dif maxP (ProtocolString lvl) reduce) -> [("Entity Id",showText eid),("Gamemode", showText gm),("Dimension",showText dim),("Difficulty",showText dif),("Max Players",showText maxP),("Level Type",T.pack lvl)] ++ if reduce then [("Reduce Debug Info","")] else []
instance Serial JoinGame where
  -- For whatever reason, we need the eid as an Int32 here, not a VarInt
  serialize (JoinGame eid gamemode dim dif maxp leveltype reduce) = serialize (unVarInt (unEID eid)) *> serialize gamemode *> serialize dim *> serialize dif *> serialize maxp *> serialize leveltype *> serialize reduce
  deserialize = JoinGame <$> (EntityId . VarInt <$> deserialize) <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize
  
-- Flags bitfield, fly speed, fov modifier
data PlayerAbilities = PlayerAbilities AbilityFlags Float Float deriving (Generic,Serial)
playerAbilities :: VarInt -> OutboundPacketDescriptor PlayerAbilities
playerAbilities = defaultDescriptor Playing "PlayerAbilities" $ \(PlayerAbilities (AbilityFlags i f af c) flySpeed fovMod) -> u i "Invulnerable" ++ u f "Flying" ++ u af "Allow Flying" ++ u c "Creative" ++ [("Flying Speed",showText flySpeed),("FOV Modifier",showText fovMod)]
    where
      u b s = if b then [(s,"")] else []

{-
instance Serial PlayerAbilities where
  serialize (PlayerAbilities flag fly fov) = serialize flag *> serialize fly *> serialize fov
  deserialize = PlayerAbilities <$> deserialize <*> deserialize <*> deserialize
-}

-- x,y,z, yaw,pitch, relativity flags, TPconfirm Id
data PlayerListItem a = PlayerListItem (ProtocolList VarInt (UUID,PlayerListAction a))
playerListItem :: PlayerListActionEnum a => VarInt -> OutboundPacketDescriptor (PlayerListItem a)
playerListItem = defaultDescriptor Playing "PlayerListItem" $ \(PlayerListItem (ProtocolList actions)) -> (\(u,a) -> [("UUID",showText u)] ++ showPlayerListAction a) =<< actions

-- Helper kind for PlayerListAction
data PlayerListActionType = AddPlayer | UpdateGamemode | UpdateLatency | UpdateName | RemovePlayer

class (Serial (PlayerListAction a)) => PlayerListActionEnum a where 
  playerListActionEnum :: VarInt
  showPlayerListAction :: PlayerListAction a -> [(Text,Text)]

instance PlayerListActionEnum 'AddPlayer where 
  playerListActionEnum = 0
  showPlayerListAction (PlayerListAdd (ProtocolString name) (ProtocolList props) gm ping (ProtocolOptional mDispName)) = [("Action","Add Player"),("Name",T.pack name),("Properties",showText props),("Gamemode",showText gm),("Ping",showText ping)] ++ (case mDispName of {Just d -> [("Display Name",showText . unProtocolString $ d)]; Nothing -> []})

instance PlayerListActionEnum 'UpdateGamemode where 
  playerListActionEnum = 1
  showPlayerListAction (PlayerListGamemode gm) = [("Action","Update Gamemode"),("Gamemode",showText gm)]

instance PlayerListActionEnum 'UpdateLatency where 
  playerListActionEnum = 2
  showPlayerListAction (PlayerListLatency ping) = [("Action","Update Latency"),("Ping",showText ping <> "ms")]

instance PlayerListActionEnum 'UpdateName where 
  playerListActionEnum = 3
  showPlayerListAction (PlayerListName (ProtocolOptional mDispName)) = [("Action","Update Name")] ++ (case mDispName of {Just d -> [("Display Name",showText . unProtocolString $ d)]; Nothing -> []})

instance PlayerListActionEnum 'RemovePlayer where 
  playerListActionEnum = 4
  showPlayerListAction PlayerListRemove = [("Action","Remove")]

-- Used in PlayerListItem
data PlayerListAction (a :: PlayerListActionType) where
  PlayerListAdd :: ProtocolString -> ProtocolList VarInt AuthProperty -> Gamemode -> VarInt -> ProtocolOptional ProtocolString -> PlayerListAction 'AddPlayer
  PlayerListGamemode :: Gamemode -> PlayerListAction 'UpdateGamemode
  PlayerListLatency :: VarInt -> PlayerListAction 'UpdateLatency
  PlayerListName :: ProtocolOptional ProtocolString -> PlayerListAction 'UpdateName
  PlayerListRemove :: PlayerListAction 'RemovePlayer

-- NOTE: Boring Serial instances
-- Haskell GADTs with type indicies don't support deriving Generic, even something like:
--
--   deriving instance (PlayerListAction 'AddPlayer)
--     
-- So we just have to write out these serial instances the boring long way

instance Serial (PlayerListAction 'AddPlayer) where
  serialize (PlayerListAdd name props gm ping mDispName) = serialize name *> serialize props *> serialize gm *> serialize ping *> serialize mDispName
  deserialize = PlayerListAdd <$> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize

instance Serial (PlayerListAction 'UpdateGamemode) where
  serialize (PlayerListGamemode gm) = serialize gm
  deserialize = PlayerListGamemode <$> deserialize @Gamemode

instance Serial (PlayerListAction 'UpdateLatency) where
  serialize (PlayerListLatency ping) = serialize ping
  deserialize = PlayerListLatency <$> deserialize @VarInt

instance Serial (PlayerListAction 'UpdateName) where
  serialize (PlayerListName mDisp) = serialize mDisp
  deserialize = PlayerListName <$> deserialize @(ProtocolOptional ProtocolString)

instance Serial (PlayerListAction 'RemovePlayer) where
  serialize PlayerListRemove = pure ()
  deserialize = pure PlayerListRemove

instance PlayerListActionEnum a => Serial (PlayerListItem a) where 
  serialize (PlayerListItem acts) = serialize (playerListActionEnum @a) *> serialize acts
  deserialize = error "Undefined: deserialize @PlayerListActionEnum"

data PlayerPositionAndLook = PlayerPositionAndLook (Double,Double,Double) (Float,Float) Word8 TPConfirmId
playerPositionAndLook :: VarInt -> OutboundPacketDescriptor PlayerPositionAndLook
playerPositionAndLook = defaultDescriptor Playing "PlayerPositionAndLook" $ pp
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
updateMetadata :: VarInt -> OutboundPacketDescriptor UpdateMetadata
updateMetadata = defaultDescriptor Playing "UpdateMetadata" $ \(UpdateMetadata _eid _mDats) -> []
  -- TODO: subclass?
instance Serial UpdateMetadata where
  serialize (UpdateMetadata eid mDats) = serialize eid *> serialize mDats
  deserialize = UpdateMetadata <$> deserialize @EntityId <*> deserialize @EntityPropertySet

-- Block pos of player spawn
data SpawnPosition = SpawnPosition BlockCoord
spawnPosition :: VarInt -> OutboundPacketDescriptor SpawnPosition
spawnPosition = defaultDescriptor Playing "SpawnPosition" $ \(SpawnPosition bc) -> [("Spawn",showText bc)]
instance Serial SpawnPosition where
  serialize (SpawnPosition pos) = serialize pos
  deserialize = SpawnPosition <$> deserialize @BlockCoord

-- All packets have their length and pktId annotated
-- serialize pkt = BS.append (serialize $ ,packetId @PlayerPositionAndLook) $ case pkt of

--instance Show Packet where
  --show pkt = formatPacket (packetName pkt) $ case pkt of

openNewWindow :: (SendsPackets r,HasPlayer r,Window w) => w -> ProtocolString -> Eff r WindowId
openNewWindow winType title = do
  wid <- usingPlayer $ \t -> do
    p <- readTVar t
    writeTVar t p {windows = Map.insert (nextWid p) (some winType) (windows p),nextWid = succ (nextWid p)}
    return (nextWid p)
  sendPacket (openWindow 0x13) (OpenWindow wid (some winType) title Nothing)
  return wid

openWindowWithItems :: (HasWorld r,SendsPackets r,HasPlayer r,Logs r,Window w) => w -> ProtocolString -> TVar Inventory -> Eff r WindowId
openWindowWithItems (ty :: tyT) name tI = do
  wid <- openNewWindow ty name
  playerInv <- Map.mapKeysMonotonic (+(27 - 9)) . playerInventory <$> getPlayer
  items <- Map.union playerInv <$> (send . WorldSTM $ readTVar tI)
  logp $ "Sending window " <> T.pack (show wid) <> " of type " <> T.pack (windowIdentifier @tyT) <> " with items " <> T.pack (show items)
  -- This is wrong because it doesn't pad between items
  sendPacket (windowItems 0x14) (WindowItems wid (ProtocolList $ Map.elems items) {-(slotCount @tyT)-})
  return wid

-- Add a new tid to the que
pendTeleport :: (SendsPackets r,Logs r,HasPlayer r) => (Double,Double,Double) -> (Float,Float) -> Word8 -> Eff r ()
pendTeleport xyz yp relFlag = do
  p <- usingPlayer $ \t -> do
    p <- readTVar t
    writeTVar t p {nextTid = 1 + nextTid p,teleportConfirmationQue = Set.insert (nextTid p) $ teleportConfirmationQue p}
    return p
  sendPacket (playerPositionAndLook 0x2F) (PlayerPositionAndLook xyz yp relFlag (nextTid p))

-- Check if the tid is in the que. If it is, then clear and return true, else false
clearTeleport :: HasPlayer r => VarInt -> Eff r Bool
clearTeleport tid = usingPlayer $ \t -> do
  p <- readTVar t 
  writeTVar t p {teleportConfirmationQue = Set.delete tid $ teleportConfirmationQue p}
  return (Set.member tid $ teleportConfirmationQue p)

-- Set the player's gamemode
setGamemode :: (SendsPackets r,Logs r,HasPlayer r) => Gamemode -> Eff r ()
setGamemode g = do
  overPlayer $ \p -> p {gameMode = g}
  sendPacket (changeGameState 0x1E) (ChangeGameState (ChangeGamemode g))
  case g of
    Survival -> sendPacket (playerAbilities 0x2C) (PlayerAbilities (AbilityFlags False False False False) 0 1)
    Creative -> sendPacket (playerAbilities 0x2C) (PlayerAbilities (AbilityFlags True False True True) 0 1)

-- Ideal version:
--
-- sendPacket (ChangeGameState (ChangeGamemode g))
--

-- TODO: Semantic slot descriptors
setInventorySlot :: (SendsPackets r,Logs r, HasPlayer r) => Short -> Slot -> Eff r ()
setInventorySlot slotNum slotData = do
  overPlayer $ \p -> p {playerInventory = setSlot slotNum slotData (playerInventory p)}
  sendPacket (setSlot 0x16) (SetSlot 0 slotNum slotData)

{-# INLINE colPacket #-}
colPacket :: HasWorld r => (Int,Int) -> Maybe BS.ByteString -> Eff r ChunkData
colPacket (cx,cz) mbio = forM [0..15] (\cy -> getChunk (ChunkCoord (cx,cy,cz))) >>= \cs -> return (chunksToColumnPacket cs (cx,cz) mbio)

chunksToColumnPacket :: [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> ChunkData
chunksToColumnPacket cs (cx,cz) mbio = ChunkData (fromIntegral cx,fromIntegral cz) True (bitMask cs) (filter (not . isAirChunk) cs) mbio (ProtocolList [])
  where
    -- [Bool] -> VarInt basically
    bitMask as = foldr (\b i -> fromBool b .|. shiftL i 1) 0 (map (not . isAirChunk) as)
    fromBool True = 1
    fromBool False = 0
    isAirChunk (ChunkSection m) = Map.null m

runWorld :: (Logs r, PerformsIO r) => MVar WorldData -> Eff (World ': r) a -> Eff r a
runWorld _ (Pure x) = Pure x
runWorld w' (Eff u q) = case u of
  Inject (ForkWorld e) -> runWorld w' (runTCQ q (runWorld w' e))
  Inject (GetChunk chunk) -> runWorld w' . runTCQ q . Map.findWithDefault (ChunkSection Map.empty) chunk . chunks =<< send (readMVar w')
  Inject (RemoveBlock bc) -> do
    send $ modifyMVar_ w' $ \w -> do
      let f = Map.delete (blockToRelative bc)
      return w {chunks = Map.alter (Just . maybe (ChunkSection $ f Map.empty) (\(ChunkSection c) -> ChunkSection $ f c)) (blockToChunk bc) (chunks w)}
    runWorld w' $ broadcastPacket (blockChange 0x0B) (BlockChange bc (some Air))
    runWorld w' (runTCQ q ())
  Inject (SetBlock b' bc) -> do
    send $ modifyMVar_ w' $ \w -> do
      let f = Map.insert (blockToRelative bc) b'
      return w {chunks = Map.alter (Just . maybe (ChunkSection $ f Map.empty) (\(ChunkSection c) -> ChunkSection $ f c)) (blockToChunk bc) (chunks w)}
    runWorld w' $ broadcastPacket (blockChange 0x0B) (BlockChange bc b')
    runWorld w' (runTCQ q ())
  Inject (SetChunk c' cc@(ChunkCoord (x,y,z))) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = Map.insert cc c' (chunks w)}
    runWorld w' $ broadcastPacket (chunkData 0x20) (ChunkData (fromIntegral x,fromIntegral z) False (bit y) [c'] Nothing (ProtocolList []))
    runWorld w' (runTCQ q ())
  Inject (SetColumn col' (cx,cz) mBio) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = fst $ foldl (\(m,i) c -> (Map.insert (ChunkCoord (cx,i,cz)) c m,i + 1)) (chunks w,0) col'}
    runWorld w' $ broadcastPacket (chunkData 0x20) $ chunksToColumnPacket col' (cx,cz) mBio
    runWorld w' (runTCQ q ())
  Inject FreshEID -> (>>= runWorld w' . runTCQ q) . send . modifyMVar w' $ \w -> return (w {nextEID = succ $ nextEID w},nextEID w)
  Inject FreshUUID -> (>>= runWorld w' . runTCQ q) . send . modifyMVar w' $ \w -> return (w {nextUUID = incUUID $ nextUUID w},nextUUID w)
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  -- Warning: throws error if player not found
  Inject (GetPlayer i) -> runWorld w' . runTCQ q =<< send . readTVarIO . flip (Map.!) i . players =<< send (readMVar w')
  -- Warning: throws error if player not found
  Inject (SetPlayer i p) -> do
    send . atomically . flip writeTVar p . flip (Map.!) i . players =<< send (readMVar w')
    runWorld w' (runTCQ q ())
  Inject (NewPlayer t) -> do
    pid <- runWorld w' freshEID
    send $ modifyMVar_ w' $ \w -> return w {players = Map.insert pid t $ players w}
    runWorld w' (runTCQ q pid)
  Inject (GetEntity e) -> send (readMVar w') >>= runWorld w' . runTCQ q . flip (Map.!) e . entities
  Inject (DeleteEntity e) -> send (modifyMVar_ w' $ \w -> return w {entities = Map.delete e . entities $ w}) >> runWorld w' (runTCQ q ())
  -- Pattern match on SuchThat to reinfer the constrain `Mob` into the contraint `Entity` for storage in the entities map
  Inject (SummonMob (SuchThat m)) -> do
    (eid,uuid) <- send . modifyMVar w' $ \w ->
      return (w {entities = Map.insert (nextEID w) (SuchThat m) (entities w),nextEID = succ (nextEID w),nextUUID = incUUID (nextUUID w)},(nextEID w,nextUUID w))
    runWorld w' $ broadcastPacket (spawnMob 0x03) $ makeSpawnMob eid uuid 0 (SuchThat m)
    runWorld w' (runTCQ q ())
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  Inject (SummonObject (SuchThat m)) -> do
    (eid,uuid) <- send . modifyMVar w' $ \w ->
      return (w {entities = Map.insert (nextEID w) (SuchThat m) (entities w),nextEID = succ (nextEID w),nextUUID = incUUID (nextUUID w)},(nextEID w,nextUUID w))
    runWorld w' $ broadcastPacket (spawnObject 0x00) $ SpawnObject eid uuid (SuchThat m)
    runWorld w' $ broadcastPacket (updateMetadata 0x3C) $ UpdateMetadata eid (EntityPropertySet $ map Just $ entityMeta (runIdentity m))
    runWorld w' (runTCQ q ())
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  Inject AllPlayers -> runWorld w' . runTCQ q =<< send . atomically . traverse readTVar . Map.elems . players =<< send (readMVar w')
  --Inject (ForallPlayers f) -> do
    --send $ modifyMVar_ w' $ \w -> return w {players = fmap f (players w)}
    --runWorld w' (runTCQ q ())
  Inject (BroadcastPacket pkt) -> do
    send . atomically . mapM_ (\p -> readTVar p >>= \pd -> case playerState pd of {Playing -> flip writeTQueue pkt . packetQueue $ pd; _ -> pure ()}) . players =<< send (readMVar w')
    runWorld w' (runTCQ q ())
  Inject (WorldSTM stm) -> runWorld w' . runTCQ q =<< send (atomically stm)
  Weaken u' -> Eff u' (Singleton (runWorld w' . runTCQ q))
