{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- | This module provides utilities for writing serverbound packets @'InboundPacketDescriptor'@s to go with them,
-- as well as an extensive library of vanilla Minecraft packets and descriptors.
module Civskell.Packet.Clientbound
  (
  -- * Utilties for making packets
  defaultDescriptor
  -- * Vanilla clientbound packets
  -- ** Handshaking
  ,LegacyHandshakePong(..),legacyHandshakePong
  -- ** Playing
  -- *** Spawn Object
  ,SpawnObject(..),spawnObject
  -- *** Spawn Experience Orb
  ,SpawnExpOrb(..),spawnExpOrb
  -- *** Spawn Global Entity
  ,SpawnGlobalEntity(..),spawnGlobalEntity
  -- *** Spawn Mob
  ,SpawnMob(..),spawnMob
  ,makeSpawnMob
  -- *** Spawn Painting
  ,SpawnPainting(..),spawnPainting
  -- *** Spawn Player
  ,SpawnPlayer(..),spawnPlayer
  -- *** Animation
  ,Animation(..),animation
  -- *** Statistics
  ,Statistics(..),statistics
  -- *** Block Break Animation
  ,BlockBreakAnimation(..),blockBreakAnimation
  -- *** Update Block Entity
  ,UpdateBlockEntity(..),updateBlockEntity
  -- *** Block Action
  ,BlockAction(..),blockAction
  -- *** Block Change
  ,BlockChange(..),blockChange
  -- *** Boss Bar
  ,BossBar(..),bossBar
  -- *** Server Difficulty
  ,ServerDifficulty(..),serverDifficulty
  -- *** Tab-Complete
  ,TabComplete(..),tabComplete
  -- *** Chat Message
  ,ChatMessage(..),chatMessage
  -- *** Multi Block Change
  ,MultiBlockChange(..),multiBlockChange
  -- *** Confirm Transaction
  ,ConfirmTransaction(..),confirmTransaction
  -- *** Close Window
  ,CloseWindow(..),closeWindow
  -- *** Open Window
  ,OpenWindow(..),openWindow
  -- *** Window Items
  ,WindowItems(..),windowItems
  -- *** Window Property
  ,WindowProperty(..),windowProperty
  -- *** Set Slot
  ,SetSlot(..),setSlot
  -- *** Set Cooldown
  ,SetCooldown(..),setCooldown
  -- *** Plugin Message
  ,PluginMessage(..),pluginMessage
  -- *** Named Sound Effect
  ,NamedSoundEffect(..),namedSoundEffect
  -- *** Disconnect (play)
  ,DisconnectPlay(..),disconnectPlay
  -- *** Entity Status
  ,EntityStatus(..),entityStatus
  -- *** Explosion
  ,Explosion(..),explosion
  -- *** Unload Chunk
  ,UnloadChunk(..),unloadChunk
  -- *** Change Game State
  ,ChangeGameState(..),changeGameState
  -- *** Keep Alive
  ,KeepAlive(..),keepAlive
  -- *** Chunk Data
  ,ChunkData(..),chunkData
  ,colPacket
  ,chunksToColumnPacket
  -- *** Effect
  ,Effect(..),effect
  -- *** Particle
  -- *** Join Game
  ,JoinGame(..),joinGame
  -- *** Map
  -- *** Entity
  -- *** Entity Relative Move
  -- *** Entity Look And Relative Move
  -- *** Entity Look
  -- *** Vehicle Move
  -- *** Open Sign Editor
  -- *** Craft Recipe Response
  -- *** Player Abilities
  ,PlayerAbilities(..),playerAbilities
  -- *** Combat Event
  -- *** Player List Item
  ,PlayerListItem(..),playerListItem
  ,PlayerListActionType(..)
  ,PlayerListAction(..)
  ,PlayerListActionEnum(..)
  -- *** Player Position And Look
  ,PlayerPositionAndLook(..),playerPositionAndLook
  -- *** Use Bed
  -- *** Unlock Recipes
  -- *** Destroy Entities
  -- *** Remove Entity Effect
  -- *** Resource Pack Send
  -- *** Respawn
  -- *** Entity Head Look
  -- *** Select Advancement Tab
  -- *** World Border
  -- *** Camera
  -- *** Held Item Change
  -- *** Display Scoreboard
  -- *** Entity Metadata
  ,UpdateMetadata(..),updateMetadata
  -- *** Attach Entity
  -- *** Entity Velocity
  -- *** Entity Equipment
  -- *** Set Experience
  -- *** Update Health
  -- *** Scoreboard Objective
  -- *** Set Passengers
  -- *** Teams
  -- *** Update Score
  -- *** Spawn Position
  ,SpawnPosition(..),spawnPosition
  -- *** Time Update
  -- *** Title
  -- *** Sound Effect
  -- *** Player List Header And Footer
  -- *** Collect Item
  -- *** Entity Teleport
  -- *** Advancements
  -- *** Entity Properties
  -- *** Entity Effect

  -- ** Status
  -- *** Response
  ,StatusResponse(..),statusResponse
  -- *** Pong
  ,StatusPong(..),statusPong

  -- ** Login
  -- *** Disconnect (login)
  ,Disconnect(..),disconnect
  -- *** Encryption Request
  ,EncryptionRequest(..),encryptionRequest
  -- *** Login Success
  ,LoginSuccess(..),loginSuccess
  -- *** Set Compression
  ,SetCompression(..),Civskell.Packet.Clientbound.setCompression
  ) where
import Crypto.Hash (hash,Digest,SHA1)
import Data.Text (Text)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad
import Control.Lens
import qualified Data.Text as T
import Data.Semigroup
import Data.Bits
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Maybe (fromMaybe)
import Data.Int
import Data.SuchThat
import Data.Word
import GHC.Generics (Generic)
import Numeric (showHex)
import qualified Data.ByteString as BS
import Unsafe.Coerce

import Civskell.Data.Types
import Civskell.Data.Util

-- | A sane default way to create a @'VarInt' -> 'OutboundPacketDescriptor' p@ given some meta information. It uses
-- the supplied information, plus the @'Serial'@ instance's @'serialize'@ for @'serializePacket'@. The @'VarInt'@
-- argument is left at the end as a temporary kludge to make packets work. It will hopefully be replaced with a more
-- elegant solution (like reading from the @'Configuration'@ somehow) in the future. As it stands, you must provide
-- the packet id *at the use site* of the descriptor.
defaultDescriptor :: Serial p => ServerState -> T.Text -> (p -> [(T.Text,T.Text)]) -> VarInt -> OutboundPacketDescriptor p
defaultDescriptor ss name pret pktId = PacketDescriptor 
  {packetState = ss
  ,packetName = name 
  ,packetPretty = pret
  ,packetHandler = PacketSerializer {packetId = pktId,serializePacket = serialize}
  }

-- | The vanilla Minecraft clientbound Legacy Handshake Pong packet. 
-- This is an old-style ping response so we can tell old clients what's up.
-- See @'legacyHandshakePongConstant'@ for more information.
data LegacyHandshakePong = LegacyHandshakePong

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'LegacyHandshakePong'@ packets.
legacyHandshakePong :: VarInt -> OutboundPacketDescriptor LegacyHandshakePong
legacyHandshakePong = defaultDescriptor Handshaking "LegacyHandshakePong" $ const []

-- | This instance is special because of the @'legacyHandshakePongConstant'@.
instance Serial LegacyHandshakePong where
  serialize LegacyHandshakePong = putByteString legacyHandshakePongConstant
  deserialize = getByteString (BS.length legacyHandshakePongConstant) >>= \b -> if b /= legacyHandshakePongConstant then fail "Failed: deserialize @LegacyHandshakePong" else pure LegacyHandshakePong

-- | The vanilla Minecraft clientbound [Disconnect (login)](http://wiki.vg/Protocol#Disconnect_.28login.29) packet.
-- Not to be confused with @'DisconnectPlay'@.
data Disconnect = Disconnect ProtocolString deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'Disconnect'@ packets.
disconnect :: VarInt -> OutboundPacketDescriptor Disconnect
disconnect = defaultDescriptor LoggingIn "Disconnect (login)" $ \(Disconnect reason) -> [("Reason",showText . unProtocolString $ reason)]

-- Server ID, Pub Key, Verify Token
-- | The vanilla Minecraft clientbound [Encryption Request](http://wiki.vg/Protocol#Encryption_Request) packet.
data EncryptionRequest = EncryptionRequest ProtocolString LengthAnnotatedByteString LengthAnnotatedByteString deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'EncryptionRequest'@ packets.
encryptionRequest :: VarInt -> OutboundPacketDescriptor EncryptionRequest
encryptionRequest = defaultDescriptor LoggingIn "Encryption Request" $ \(EncryptionRequest (ProtocolString sId) (LengthAnnotatedByteString pubKey) (LengthAnnotatedByteString vt)) -> [("Server Id",T.pack sId),("Public Key Hash",(T.take 7 $ showText (hash pubKey :: Digest SHA1)) <> "..."),("Verify Token",T.pack $ "0x" ++ (flip showHex "" =<< BS.unpack vt))]

-- UUID (with hyphens), Username
-- | The vanilla Minecraft clientbound [Login Success](http://wiki.vg/Protocol#Login_Success) packet.
data LoginSuccess = LoginSuccess ProtocolString ProtocolString deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'LoginSuccess'@ packets.
loginSuccess :: VarInt -> OutboundPacketDescriptor LoginSuccess
loginSuccess = defaultDescriptor LoggingIn "Login Success" $ \(LoginSuccess (ProtocolString uuid) (ProtocolString name)) -> [("UUID",T.pack uuid),("Username",T.pack name)]

-- Size threshold for compression
-- | The vanilla Minecraft clientbound [Set Compression](http://wiki.vg/Protocol#Set_Compression) packet.
data SetCompression = SetCompression VarInt deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SetCompression'@ packets.
setCompression :: VarInt -> OutboundPacketDescriptor SetCompression
setCompression = defaultDescriptor LoggingIn "Set Compression" $ \(SetCompression thresh) -> [("Compression Threshold",showText thresh)]

-- JSON String (for now)
-- | The vanilla Minecraft clientbound status [Response](http://wiki.vg/Protocol#Response) packet.
data StatusResponse = StatusResponse ProtocolString deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'StatusResponse'@ packets.
statusResponse :: VarInt -> OutboundPacketDescriptor StatusResponse
statusResponse = defaultDescriptor Status "Status Response" $ \(StatusResponse (ProtocolString statusJSON)) -> [("Status JSON",T.pack $ take 200 statusJSON)]
  -- Beware: statusJSON includes a base64 encoded png, so it is very very long

-- Payload (unique number obtained from client)
-- | The vanilla Minecraft clientbound status [Pong](http://wiki.vg/Protocol#Pong) packet.
data StatusPong = StatusPong Int64 deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'StatusPong'@ packets.
statusPong :: VarInt -> OutboundPacketDescriptor StatusPong
statusPong = defaultDescriptor Status "Status Pong" $ \(StatusPong pongTok) -> [("Pong Token",T.pack $ "0x" ++ showHex pongTok "")]

-- EID, UUID, Type of Object, x, y, z, pitch, yaw, Object Data, velx, vely, velz
-- | The vanilla Minecraft clientbound [Spawn Object](http://wiki.vg/Protocol#Spawn_Object) packet.
data SpawnObject = SpawnObject EntityId UUID (Satisfies Object)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SpawnObject'@ packets.
spawnObject :: VarInt -> OutboundPacketDescriptor SpawnObject
spawnObject = defaultDescriptor Playing "Spawn Object" $ pp
  where
    pp (SpawnObject eid uuid (SuchThat (Identity (e :: o)))) = [("Entity Id",showText eid),("UUID",showText uuid),("Type",objectName @o),("Position",showText (x,y,z)),("Looking",showText (yaw,pitch)),("Data",showText d)] ++ v
      where
        v = case mVel of
          Just (EntityVelocity (dx,dy,dz)) -> [("Velocity",showText (dx,dy,dz))]
          Nothing -> []
        (d,mVel) = objectData e
        EntityLocation (x,y,z) (yaw,pitch) = objectLocation e

-- | This instance is special because serializing a @'Satisfies'@ is hard.
instance Serial SpawnObject where
  serialize (SpawnObject eid uuid (SuchThat (Identity (e :: o)))) = serialize eid *> serialize uuid *> serialize (objectId @o) *> serialize x *> serialize y *> serialize z *> serialize pitch *> serialize yaw *> dat
    where
      dat = serialize d *> (case mVel of {Just (EntityVelocity (dx,dy,dz)) -> serialize dx *> serialize dy *> serialize dz; Nothing -> serialize (0 :: Short) *> serialize (0 :: Short) *> serialize (0 :: Short)})
      (d,mVel) = objectData e
      EntityLocation (x,y,z) (yaw,pitch) = objectLocation e
  deserialize = error "Undefined: deserialize @SpawnObject"

-- EID, x, y, z, count
-- | The vanilla Minecraft clientbound [Spawn Experience Orb](http://wiki.vg/Protocol#Spawn_Experience_Orb) packet.
data SpawnExpOrb = SpawnExpOrb EntityId (Double,Double,Double) Short deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SpawnExpOrb'@ packets.
spawnExpOrb :: VarInt -> OutboundPacketDescriptor SpawnExpOrb
spawnExpOrb = defaultDescriptor Playing "Spawn Experience Orb" $ \(SpawnExpOrb _ _ _) -> []

-- EID, Type (always 1 for thunderbolt), x, y, z
-- | The vanilla Minecraft clientbound [Spawn Global Entity](http://wiki.vg/Protocol#Spawn_Global_Entity) packet.
data SpawnGlobalEntity = SpawnGlobalEntity EntityId Word8 (Double,Double,Double) deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SpawnGlobalEntity'@ packets.
spawnGlobalEntity :: VarInt -> OutboundPacketDescriptor SpawnGlobalEntity
spawnGlobalEntity = defaultDescriptor Playing "Spawn Global Entity" $ \(SpawnGlobalEntity _ _ _) -> []

-- EID, UUID, Type, x, y, z, yaw, pitch, head pitch, velx, vely, velz, Metadata
-- data SpawnMob = SpawnMob EntityId UUID (Satisfies Entity) Word8 EntityPropertySet deriving (Generic,Serial)
-- | The vanilla Minecraft clientbound [Spawn Mob](http://wiki.vg/Protocol#Spawn_Mob) packet.
data SpawnMob = SpawnMob EntityId UUID VarInt (Double,Double,Double) (Word8,Word8,Word8) (Short,Short,Short) EntityPropertySet deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SpawnMob'@ packets.
spawnMob :: VarInt -> OutboundPacketDescriptor SpawnMob
spawnMob = defaultDescriptor Playing "Spawn Mob" $ \(SpawnMob eid uuid mobType pos look vel _props) -> [("Entity Id",showText eid),("UUID",showText uuid),("Type",showText mobType),("Position",showText pos),("Looking",showText look),("Velocity",showText vel)]

-- | Helper function for easily constructing a @'SpawnMob'@ packet.
makeSpawnMob :: EntityId -> UUID -> Word8 -> Satisfies Entity -> SpawnMob
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
-- | The vanilla Minecraft clientbound [Spawn Painting](http://wiki.vg/Protocol#Spawn_Painting) packet.
data SpawnPainting = SpawnPainting EntityId UUID String BlockCoord Word8 deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SpawnPainting'@ packets.
spawnPainting :: VarInt -> OutboundPacketDescriptor SpawnPainting
spawnPainting = defaultDescriptor Playing "Spawn Painting" $ \(SpawnPainting _ _ _ _ _) -> []

-- EID, UUID, x, y, z, yaw, pitch, metadata
-- | The vanilla Minecraft clientbound [Spawn Player](http://wiki.vg/Protocol#Spawn_Player) packet.
data SpawnPlayer = SpawnPlayer EntityId UUID (Double,Double,Double) Word8 Word8 EntityPropertySet deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SpawnPlayer'@ packets.
spawnPlayer :: VarInt -> OutboundPacketDescriptor SpawnPlayer
spawnPlayer = defaultDescriptor Playing "Spawn Player" $ \(SpawnPlayer _ _ _ _ _ _) -> []

-- EID, Animation ID (from table)
-- | The vanilla Minecraft clientbound [Animation](http://wiki.vg/Protocol#Animation_.28clientbound.29) packet.
data Animation = Animation EntityId Word8 deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'Animation'@ packets.
animation :: VarInt -> OutboundPacketDescriptor Animation
animation = defaultDescriptor Playing "Animation" $ \(Animation eid anim) -> [("Entity Id",showText eid),("Animation",showText anim)]

-- List of all stats
-- | The vanilla Minecraft clientbound [Statistics](http://wiki.vg/Protocol#Statistics) packet.
data Statistics = Statistics (ProtocolList VarInt (ProtocolString,VarInt)) deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'Statistics'@ packets.
statistics :: VarInt -> OutboundPacketDescriptor Statistics
statistics = defaultDescriptor Playing "Statistics" $ \(Statistics _) -> []

-- EID, Block coord, stage (0-9)
-- | The vanilla Minecraft clientbound [Block Break Animation](http://wiki.vg/Protocol#Block_Break_Animation) packet.
data BlockBreakAnimation = BlockBreakAnimation EntityId BlockCoord Word8 deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'BlockBreakAnimation'@ packets.
blockBreakAnimation :: VarInt -> OutboundPacketDescriptor BlockBreakAnimation
blockBreakAnimation = defaultDescriptor Playing "Block Break Animation" $ \(BlockBreakAnimation _ _ _) -> []

-- | The vanilla Minecraft clientbound [Update Block Entity](http://wiki.vg/Protocol#Update_Block_Entity) packet.
data UpdateBlockEntity = UpdateBlockEntity BlockCoord Word8 ProtocolNBT deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'UpdateBlockEntity'@ packets.
updateBlockEntity :: VarInt -> OutboundPacketDescriptor UpdateBlockEntity
updateBlockEntity = defaultDescriptor Playing "Update Block Entity" $ \(UpdateBlockEntity _ _ _) -> []

-- Block coord, Action Id (enum), Action Param, Block Type ; http://wiki.vg/Block_Actions
-- | The vanilla Minecraft clientbound [Block Action](http://wiki.vg/Protocol#Block_Action) packet.
data BlockAction = BlockAction BlockCoord (Word8,Word8) VarInt deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'BlockAction'@ packets.
blockAction :: VarInt -> OutboundPacketDescriptor BlockAction
blockAction = defaultDescriptor Playing "Block Action" $ \(BlockAction _ _ _) -> []

-- Block coord, Block ID (from global palette)
-- | The vanilla Minecraft clientbound [Block Change](http://wiki.vg/Protocol#Block_Change) packet.
data BlockChange = BlockChange BlockCoord WireBlock deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'BlockChange'@ packets.
blockChange :: VarInt -> OutboundPacketDescriptor BlockChange
blockChange = defaultDescriptor Playing "Block Change" $ \(BlockChange bc bs) -> [("Block",showText bc),("New State",showText bs)]

-- UUID, Action (from enum)
-- | The vanilla Minecraft clientbound [Boss Bar](http://wiki.vg/Protocol#Boss_Bar) packet.
-- Broken ATM, don't use it.
data BossBar = BossBar String deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'BossBar'@ packets.
bossBar :: VarInt -> OutboundPacketDescriptor BossBar
bossBar = defaultDescriptor Playing "Boss Bar" $ \(BossBar _) -> []

-- Difficulty (0-3)
-- | The vanilla Minecraft clientbound [Server Difficulty](http://wiki.vg/Protocol#Server_Difficulty) packet.
data ServerDifficulty = ServerDifficulty Difficulty deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'ServerDifficulty'@ packets.
serverDifficulty :: VarInt -> OutboundPacketDescriptor ServerDifficulty
serverDifficulty = defaultDescriptor Playing "Server Difficulty" $ \(ServerDifficulty dif) -> [("Difficulty",showText dif)]

-- List of matches for tab completion. Prefixed with length when sent
-- | The vanilla Minecraft clientbound [Tab Complete](http://wiki.vg/Protocol#Tab-Complete_.28clientbound.29) packet.
data TabComplete = TabComplete (ProtocolList VarInt ProtocolString) deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'TabComplete'@ packets.
tabComplete :: VarInt -> OutboundPacketDescriptor TabComplete
tabComplete = defaultDescriptor Playing "Tab Complete" $ \(TabComplete _) -> []

-- JSON chat string, place to appear in (0:chatbox,1:sys msg. chatbox,2:hotbar)
-- | The vanilla Minecraft clientbound [Chat Message](http://wiki.vg/Protocol#Chat_Message.28clientbound.29) packet.
data ChatMessage = ChatMessage ProtocolString Word8 deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'ChatMessage'@ packets.
chatMessage :: VarInt -> OutboundPacketDescriptor ChatMessage
chatMessage = defaultDescriptor Playing "Chat Message" $ \(ChatMessage msg loc) -> [("Message",showText . unProtocolString $ msg),("Location",showText loc)]

-- Chunk x, Chunk z, List of (chunk relative coords in special format, Block Id (global palette))
-- | The vanilla Minecraft clientbound [Multi Block Change](http://wiki.vg/Protocol#Multi_Block_Change) packet.
data MultiBlockChange = MultiBlockChange (Int32,Int32) (ProtocolList VarInt ((Word8,Word8),VarInt)) deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'MultiBlockChange'@ packets.
multiBlockChange :: VarInt -> OutboundPacketDescriptor MultiBlockChange
multiBlockChange = defaultDescriptor Playing "Multi Block Change" $ \(MultiBlockChange _ _) -> []

-- Window Id, Transaction Id, Accepted
-- | The vanilla Minecraft clientbound [Confirm Transaction](http://wiki.vg/Protocol#Confirm_Transaction_.28clientbound.29) packet.
data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'ConfirmTransaction'@ packets.
confirmTransaction :: VarInt -> OutboundPacketDescriptor ConfirmTransaction
confirmTransaction = defaultDescriptor Playing "Confirm Transaction" $ \(ConfirmTransaction _ _ _) -> []

-- | The vanilla Minecraft clientbound [Close Window](http://wiki.vg/Protocol#Close_Window_.28clientbound.29) packet.
data CloseWindow = CloseWindow WindowId deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'CloseWindow'@ packets.
closeWindow :: VarInt -> OutboundPacketDescriptor CloseWindow
closeWindow = defaultDescriptor Playing "Close Window" $ \(CloseWindow _) -> []

-- Window Id, Window Type (Enum), JSON chat string of Window Title, Num of Slots, optionally: EID of horse
-- | The vanilla Minecraft clientbound [Open Window](http://wiki.vg/Protocol#Open_Window) packet.
data OpenWindow = OpenWindow WindowId (ForAny Window) ProtocolString (Maybe EntityId)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'OpenWindow'@ packets.
openWindow :: VarInt -> OutboundPacketDescriptor OpenWindow
openWindow = defaultDescriptor Playing "Open Window" $ \(OpenWindow wid (SuchThat (Window wDesc _)) title horseEid) -> [("Window Id",showText wid),("Window Type",windowName wDesc),("Window Title",showText . unProtocolString $ title)] ++ (case horseEid of {Just eid -> [("Horse EID",showText eid)];Nothing -> []})

-- | This instance is special because serializing @'ForAny'@s is hard. Also, the horse EID thing messes up the generics.
instance Serial OpenWindow where
  serialize (OpenWindow wid (SuchThat (Window wDesc w)) title horseEid) = serialize wid *> serialize (windowIdentifier wDesc) *> serialize title *> serialize ((unsafeCoerce :: Short -> Word8) $ slotCount wDesc w) *> (case horseEid of {Just eid -> serialize eid;Nothing -> pure ()})
  deserialize = error "Unimplemented: deserialize @OpenWindow"

-- Window Id, Slots
-- | The vanilla Minecraft clientbound [Window Items](http://wiki.vg/Protocol#Window_Items) packet.
data WindowItems = WindowItems WindowId (ProtocolList Short WireSlot) deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'WindowItems'@ packets.
windowItems :: VarInt -> OutboundPacketDescriptor WindowItems
windowItems = defaultDescriptor Playing "Window Items" $ \(WindowItems wid _slots) -> [("Window Id",showText wid)]
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
-- | The vanilla Minecraft clientbound [Window Property](http://wiki.vg/Protocol#Window_Property) packet.
data WindowProperty = WindowProperty WindowId Short Short deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'WindowProperty'@ packets.
windowProperty :: VarInt -> OutboundPacketDescriptor WindowProperty
windowProperty = defaultDescriptor Playing "Window Property" $ \(WindowProperty _ _ _) -> []

-- Window Id, Slot num, <Slot>
-- | The vanilla Minecraft clientbound [Set Slot](http://wiki.vg/Protocol#Set_Slot) packet.
data SetSlot = SetSlot WindowId Short WireSlot deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SetSlot'@ packets.
setSlot :: VarInt -> OutboundPacketDescriptor SetSlot
setSlot = defaultDescriptor Playing "Set Slot" $ \(SetSlot wid slotNum slotData) -> [("Window Id",showText wid),("Slot Number",showText slotNum),("Slot Data",showText slotData)]

-- Item Id (applies to all instances), Cooldown Ticks
-- | The vanilla Minecraft clientbound [Set Cooldown](http://wiki.vg/Protocol#Set_Cooldown) packet.
data SetCooldown = SetCooldown VarInt VarInt deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SetCooldown'@ packets.
setCooldown :: VarInt -> OutboundPacketDescriptor SetCooldown
setCooldown = defaultDescriptor Playing "Set Cooldown" $ \(SetCooldown _ _) -> []

-- Plugin Channel, Data
-- | The vanilla Minecraft clientbound [Plugin Message](http://wiki.vg/Protocol#Plugin_Message) packet.
data PluginMessage = PluginMessage ProtocolString BS.ByteString

--TODO: This hex part is very bad at indentation
-- | A reasonable default @'OutboundPacketDescriptor'@ for @'PluginMessage'@ packets.
pluginMessage :: VarInt -> OutboundPacketDescriptor PluginMessage
pluginMessage = defaultDescriptor Playing "Plugin Message" $ \(PluginMessage chan msg) -> [("Plugin Channel",T.pack $ unProtocolString chan),("Message",T.pack $ "0x" ++ (flip showHex "" =<< BS.unpack msg))]

-- | Dont use generics because it would do the sensible thing and prepend the length of the payload.
-- Mojang doesn't do this, so we don't either.
instance Serial PluginMessage where
  serialize (PluginMessage str bs) = serialize str *> putByteString bs
  deserialize = error "Cant generically deserialize a PluginMessage. Please go annoy Mojang about this because it's stupid :P"

-- Sound Name (Enum), Sound Category (Enum), weird encoding for: x,y,z, Volume, Pitch
-- | The vanilla Minecraft clientbound [Named Sound Effect](http://wiki.vg/Protocol#Named_Sound_Effect) packet.
data NamedSoundEffect = NamedSoundEffect ProtocolString VarInt (Int32,Int32,Int32) Float Float deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'NamedSoundEffect'@ packets.
namedSoundEffect :: VarInt -> OutboundPacketDescriptor NamedSoundEffect
namedSoundEffect = defaultDescriptor Playing "Named Sound Effect" $ \(NamedSoundEffect _ _ _ _ _) -> []

-- Reason (JSON chat string)
-- | The vanilla Minecraft clientbound [Disconnect (play)](http://wiki.vg/Protocol#Disconnect_.28play.29) packet.
-- Not to be confused with @'Disconnect'@ (login version).
data DisconnectPlay = DisconnectPlay ProtocolString deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'DisconnectPlay'@ packets.
disconnectPlay :: VarInt -> OutboundPacketDescriptor DisconnectPlay
disconnectPlay = defaultDescriptor Playing "Disconnect (play)" $ \(DisconnectPlay _) -> []

-- EID, Status (Enum)
-- | The vanilla Minecraft clientbound [Entity Status](http://wiki.vg/Protocol#Entity_Status) packet.
data EntityStatus = EntityStatus EntityId Word8 deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'EntityStatus'@ packets.
entityStatus :: VarInt -> OutboundPacketDescriptor EntityStatus
entityStatus = defaultDescriptor Playing "Entity Status" $ \(EntityStatus _ _) -> []

-- x,y,z, radius, affected block offsets, velocity of pushed player
-- | The vanilla Minecraft clientbound [Explosion](http://wiki.vg/Protocol#Explosion) packet.
data Explosion = Explosion (Float,Float,Float) Float [(Word8,Word8,Word8)] (Float,Float,Float) deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'Explosion'@ packets.
explosion :: VarInt -> OutboundPacketDescriptor Explosion
explosion = defaultDescriptor Playing "Explosion" $ \(Explosion _ _ _ _) -> []

-- Chunk X, Chunk Z
-- | The vanilla Minecraft clientbound [Unload Chunk](http://wiki.vg/Protocol#Unload_Chunk) packet.
data UnloadChunk = UnloadChunk (Int32,Int32) deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'UnloadChunk'@ packets.
unloadChunk :: VarInt -> OutboundPacketDescriptor UnloadChunk
unloadChunk = defaultDescriptor Playing "Unload Chunk" $ \(UnloadChunk _) -> []

-- Reason (Enum), Value (from Enum)
-- | The vanilla Minecraft clientbound [Change Game State](http://wiki.vg/Protocol#Change_Game_State) packet.
data ChangeGameState = ChangeGameState GameStateChange deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'ChangeGameState'@ packets.
changeGameState :: VarInt -> OutboundPacketDescriptor ChangeGameState
changeGameState = defaultDescriptor Playing "Change Game State" $ \(ChangeGameState _) -> []

-- Random Id <-- Prevents Timeout
-- | The vanilla Minecraft clientbound [Keep Alive](http://wiki.vg/Protocol#Keep_Alive_.28clientbound.29) packet.
data KeepAlive = KeepAlive KeepAliveId deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'KeepAlive'@ packets.
keepAlive :: VarInt -> OutboundPacketDescriptor KeepAlive
keepAlive = defaultDescriptor Playing "Keep Alive" $ \(KeepAlive kid) -> [("Keep Alive Id",showText kid)]

-- Chunk X, Chunk Z, Full Chunk?, Bitmask of slices present, [Chunk Section], optional: 256 byte array of biome data, [Block entity NBT tag]
-- | The vanilla Minecraft clientbound [Chunk Data](http://wiki.vg/Protocol#Chunk_Data) packet.
data ChunkData = ChunkData (Int32,Int32) Bool VarInt [ChunkSection WireBlock] (Maybe BS.ByteString) (ProtocolList VarInt ProtocolNBT)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'ChunkData'@ packets.
chunkData :: VarInt -> OutboundPacketDescriptor ChunkData
chunkData = defaultDescriptor Playing "Chunk Data" $ \(ChunkData (cx,cz) guCont bitMask cs _mBio _nbt) -> [("Column",showText (cx,cz)),("Bit Mask",showText bitMask)] ++ (if guCont then [("Full Chunk","")] else []) ++ [("Section Count",showText (length cs))]

-- | This instance is special because the ground-up continuous flag and the biome information are related,
-- and the chunk serialization uses the total serialized length in bytes rather than the number of chunk sections sent.
instance Serial ChunkData where
  serialize (ChunkData (cx,cz) guCont bitMask chunkSecs mBiomes blockEnts) = serialize cx *> serialize cz *> serialize guCont *> serialize bitMask *> withLength (runPutS $ (traverse serialize chunkSecs) *> maybe (pure ()) putByteString mBiomes) *> serialize @(ProtocolList VarInt ProtocolNBT) blockEnts
  deserialize = error "Undefined: deserialize @ChunkData"

-- Effect Id (Enum), block coord, extra data (from Enum), disable relative?
-- | The vanilla Minecraft clientbound [Effect](http://wiki.vg/Protocol#Effect) packet.
data Effect = Effect Int32 BlockCoord Int32 Bool deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'Effect'@ packets.
effect :: VarInt -> OutboundPacketDescriptor Effect
effect = defaultDescriptor Playing "Effect" $ \(Effect _ _ _ _) -> []

-- | The vanilla Minecraft clientbound [Join Game](http://wiki.vg/Protocol#Join_Game) packet.
data JoinGame = JoinGame EntityId Gamemode Dimension Difficulty Word8 ProtocolString Bool

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'JoinGame'@ packets.
joinGame :: VarInt -> OutboundPacketDescriptor JoinGame
joinGame = defaultDescriptor Playing "Join Game" $ \(JoinGame eid gm dim dif maxP (ProtocolString lvl) reduce) -> [("Entity Id",showText eid),("Gamemode", showText gm),("Dimension",showText dim),("Difficulty",showText dif),("Max Players",showText maxP),("Level Type",T.pack lvl)] ++ if reduce then [("Reduce Debug Info","")] else []

-- | This is a special instance because the packet needs to serialize the @'EntityId'@ as a *raw* @'Int32'@ instead of a @'VarInt'@ because Mojang.
instance Serial JoinGame where
  -- For whatever reason, we need the eid as an Int32 here, not a VarInt
  serialize (JoinGame eid gamemode dim dif maxp leveltype reduce) = serialize (unVarInt (unEID eid)) *> serialize gamemode *> serialize dim *> serialize dif *> serialize maxp *> serialize leveltype *> serialize reduce
  deserialize = JoinGame <$> (EntityId . VarInt <$> deserialize) <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize
  
-- Flags bitfield, fly speed, fov modifier
-- | The vanilla Minecraft clientbound [Player Abilities](http://wiki.vg/Protocol#Player_Abilities_.28clientbound.29) packet.
data PlayerAbilities = PlayerAbilities AbilityFlags Float Float deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'PlayerAbilities'@ packets.
playerAbilities :: VarInt -> OutboundPacketDescriptor PlayerAbilities
playerAbilities = defaultDescriptor Playing "Player Abilities" $ \(PlayerAbilities (AbilityFlags i f af c) flySpeed fovMod) -> u i "Invulnerable" ++ u f "Flying" ++ u af "Allow Flying" ++ u c "Creative" ++ [("Flying Speed",showText flySpeed),("FOV Modifier",showText fovMod)]
    where
      u b s = if b then [(s,"")] else []

{-
instance Serial PlayerAbilities where
  serialize (PlayerAbilities flag fly fov) = serialize flag *> serialize fly *> serialize fov
  deserialize = PlayerAbilities <$> deserialize <*> deserialize <*> deserialize
-}

-- x,y,z, yaw,pitch, relativity flags, TPconfirm Id
-- | The vanilla Minecraft clientbound [Player List Item](http://wiki.vg/Protocol#Player_List_Item) packet.
-- The type parameter represents what type of @'PlayerListAction'@(s) this @'PlayerListItem'@ contains, since
-- it can only be one type at a type.
data PlayerListItem a = PlayerListItem (ProtocolList VarInt (UUID,PlayerListAction a))

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'PlayerListItem'@ packets.
playerListItem :: PlayerListActionEnum a => VarInt -> OutboundPacketDescriptor (PlayerListItem a)
playerListItem = defaultDescriptor Playing "Player List Item" $ \(PlayerListItem (ProtocolList actions)) -> (\(u,a) -> [("UUID",showText u)] ++ showPlayerListAction a) =<< actions

-- | Helper kind for @'PlayerListAction'@.
data PlayerListActionType = AddPlayer | UpdateGamemode | UpdateLatency | UpdateName | RemovePlayer

-- | Reflect the value-level @'VarInt'@ and pretty printing function from a type level dictionary.
-- [See the wiki.vg docs](http://wiki.vg/Protocol#Player_List_Item) for a pretty diagram.
class (Serial (PlayerListAction a)) => PlayerListActionEnum a where 
  -- | The flag for indicating which \"Action" this is on the wire.
  playerListActionEnum :: VarInt
  -- | The pretty printing function for use in @'playerListItem'@
  showPlayerListAction :: PlayerListAction a -> [(Text,Text)]

-- | AddPlayer -> 0
instance PlayerListActionEnum 'AddPlayer where 
  playerListActionEnum = 0
  showPlayerListAction (PlayerListAdd (ProtocolString name) (ProtocolList props) gm ping (ProtocolOptional mDispName)) = [("Action","Add Player"),("Name",T.pack name),("Properties",showText props),("Gamemode",showText gm),("Ping",showText ping)] ++ (case mDispName of {Just d -> [("Display Name",showText . unProtocolString $ d)]; Nothing -> []})

-- | UpdateGamemode -> 1
instance PlayerListActionEnum 'UpdateGamemode where 
  playerListActionEnum = 1
  showPlayerListAction (PlayerListGamemode gm) = [("Action","Update Gamemode"),("Gamemode",showText gm)]

-- | UpdateLatency -> 2
instance PlayerListActionEnum 'UpdateLatency where 
  playerListActionEnum = 2
  showPlayerListAction (PlayerListLatency ping) = [("Action","Update Latency"),("Ping",showText ping <> "ms")]

-- | UpdateName -> 3
instance PlayerListActionEnum 'UpdateName where 
  playerListActionEnum = 3
  showPlayerListAction (PlayerListName (ProtocolOptional mDispName)) = [("Action","Update Name")] ++ (case mDispName of {Just d -> [("Display Name",showText . unProtocolString $ d)]; Nothing -> []})

-- | RemovePlayer -> 4
instance PlayerListActionEnum 'RemovePlayer where 
  playerListActionEnum = 4
  showPlayerListAction PlayerListRemove = [("Action","Remove")]

-- | A single player list update, annotated with which one it is. This is used in @'PlayerListItem'@ packets.
--
-- NOTE: Boring Serial instances
--
-- Haskell GADTs with type indicies don't support deriving Generic, even something like:
--
-- > deriving instance (PlayerListAction 'AddPlayer)
--     
-- So we just have to write out these serial instances the boring long way
data PlayerListAction (a :: PlayerListActionType) where
  PlayerListAdd :: ProtocolString -> ProtocolList VarInt AuthProperty -> Gamemode -> VarInt -> ProtocolOptional ProtocolString -> PlayerListAction 'AddPlayer
  PlayerListGamemode :: Gamemode -> PlayerListAction 'UpdateGamemode
  PlayerListLatency :: VarInt -> PlayerListAction 'UpdateLatency
  PlayerListName :: ProtocolOptional ProtocolString -> PlayerListAction 'UpdateName
  PlayerListRemove :: PlayerListAction 'RemovePlayer

-- | Boring
instance Serial (PlayerListAction 'AddPlayer) where
  serialize (PlayerListAdd name props gm ping mDispName) = serialize name *> serialize props *> serialize gm *> serialize ping *> serialize mDispName
  deserialize = PlayerListAdd <$> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize

-- | Boring
instance Serial (PlayerListAction 'UpdateGamemode) where
  serialize (PlayerListGamemode gm) = serialize gm
  deserialize = PlayerListGamemode <$> deserialize @Gamemode

-- | Boring
instance Serial (PlayerListAction 'UpdateLatency) where
  serialize (PlayerListLatency ping) = serialize ping
  deserialize = PlayerListLatency <$> deserialize @VarInt

-- | Boring
instance Serial (PlayerListAction 'UpdateName) where
  serialize (PlayerListName mDisp) = serialize mDisp
  deserialize = PlayerListName <$> deserialize @(ProtocolOptional ProtocolString)

-- | Boring
instance Serial (PlayerListAction 'RemovePlayer) where
  serialize PlayerListRemove = pure ()
  deserialize = pure PlayerListRemove

-- | This is an incomplete instance.
--
-- > deserialize = error "Unimplemented: deserialize @PlayerListActionEnum"
instance PlayerListActionEnum a => Serial (PlayerListItem a) where 
  serialize (PlayerListItem acts) = serialize (playerListActionEnum @a) *> serialize acts
  deserialize = error "Unimplemented: deserialize @PlayerListActionEnum"

-- TODO: verify pairs work how we assume them to
-- | The vanilla Minecraft clientbound [Player Position And Look](http://wiki.vg/Protocol#Player_Position_And_Look) packet.
data PlayerPositionAndLook = PlayerPositionAndLook (Double,Double,Double) (Float,Float) Word8 TPConfirmId deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'PlayerPositionAndLook'@ packets.
playerPositionAndLook :: VarInt -> OutboundPacketDescriptor PlayerPositionAndLook
playerPositionAndLook = defaultDescriptor Playing "Player Position And Look" $ pp
  where
    pp (PlayerPositionAndLook (x,y,z) (yaw,pitch) rel tid) =
      [("X",r 1 (showText x))
      ,("Y",r 2 (showText y))
      ,("Z",r 3 (showText z))
      ,("Yaw",r 4 (showText yaw))
      ,("Pitch",r 5 (showText pitch))
      ,("Teleport Id", showText tid)
      ] where r b = if testBit rel b then ("~" <>) else id

-- Block pos of player spawn
-- | The vanilla Minecraft clientbound [Entity Metadata](http://wiki.vg/Protocol#Entity_Metdata) packet.
data UpdateMetadata = UpdateMetadata EntityId EntityPropertySet deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'UpdateMetadata'@ packets.
updateMetadata :: VarInt -> OutboundPacketDescriptor UpdateMetadata
updateMetadata = defaultDescriptor Playing "Entity Metadata" $ \(UpdateMetadata _eid _mDats) -> []

-- Block pos of player spawn
-- | The vanilla Minecraft clientbound [Spawn Position](http://wiki.vg/Protocol#Spawn_Position) packet.
data SpawnPosition = SpawnPosition BlockCoord deriving (Generic,Serial)

-- | A reasonable default @'OutboundPacketDescriptor'@ for @'SpawnPosition'@ packets.
spawnPosition :: VarInt -> OutboundPacketDescriptor SpawnPosition
spawnPosition = defaultDescriptor Playing "Spawn Position" $ \(SpawnPosition bc) -> [("Spawn",showText bc)]

-- | Generate a @'ChunkData'@ packet given a chunk (x,y) coordinate and some biome information.
colPacket :: Member WorldManipulation r => (Int,Int) -> Maybe BS.ByteString -> Eff r ChunkData
colPacket (cx,cz) mbio = do
  -- Grab all 15 chunks, and make them in to WireBlocks
  cs <- forM [0..15] (\cy -> chunkToWireBlock . view (worldChunks . at (ChunkCoord (cx,cy,cz)) . to (fromMaybe emptyChunk)) <$> get) 
  return (chunksToColumnPacket cs (cx,cz) mbio)

-- | Generate a @'ChunkData'@ packet given some @'ChunkSection'@s that are in a serializable form, as
-- well as a chunk (x,y) coordinate and some biome information.
chunksToColumnPacket :: [ChunkSection WireBlock] -> (Int,Int) -> Maybe BS.ByteString -> ChunkData
chunksToColumnPacket cs (cx,cz) mbio = ChunkData (fromIntegral cx,fromIntegral cz) True (bitMask cs) (filter (not . isAirChunk) cs) mbio (ProtocolList [])
  where
    -- [Bool] -> VarInt basically
    bitMask as = foldr (\b i -> fromBool b .|. shiftL i 1) 0 (map (not . isAirChunk) as)
    fromBool True = 1
    fromBool False = 0
    isAirChunk (ChunkSection _m) = error "Unimplemented: Clientbound.chunksToColumnPacket.isAirChunk. Reason: Used to be map, now we have to traverse the entire array to see if it is empty" -- Map.null m


