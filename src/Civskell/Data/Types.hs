{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- TODO: This export list is pretty scary. We should make tiered modules or something
module Civskell.Data.Types
  (module Civskell.Data.Common
  ,module Civskell.Data.Logging
  ,module Civskell.Data.Types
  ,module Civskell.Data.Networking
  ) where

import Civskell.Data.Common
import Civskell.Data.Logging
import Civskell.Data.Networking

import GHC.Generics
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Data.Functor.Identity
import Data.Semigroup
import Data.Foldable
import Control.Monad.Freer
import Control.Monad.Freer.Writer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import Control.Concurrent.STM
import Control.Monad
import Crypto.Cipher.AES (AES128)
import Data.Aeson (withObject,(.:),(.:?),(.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types
import Data.Bits
import Data.Int
import Data.NBT
import Data.Text (Text)
import Data.SuchThat
import Data.String
import qualified GHC.Exts as Exts
import Data.Word
import Hexdump (prettyHex)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import qualified Data.Serialize as Cereal
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import Data.Attoparsec.ByteString

-- All the information about a mineman world. Notably, players is a Map of PId's to TVars of player data, not actual player data
data WorldData = WorldData 
  {chunks :: Map ChunkCoord ChunkSection
  ,entities :: Map EntityId (Some Entity)
  ,players :: Map PlayerId (TVar PlayerData)
  ,nextEID :: EntityId
  ,nextUUID :: UUID
  }

-- Having transactional access to the World is an effect
type WorldManipulation = State WorldData

-- Having transactional access to a single Player's data is an effect
type PlayerManipulation = State PlayerData

-- Stores all the relevant information about a player
-- TODO: Extensibility?
data PlayerData = PlayerData
  {teleportConfirmationQue :: Set.Set VarInt
  ,nextTid :: VarInt
  ,keepAliveQue :: Set.Set VarInt
  ,nextKid :: VarInt
  ,nextWid :: WindowId
  -- Map of WindowId's to that window's metadata (including accessor effects)
  --,windows :: Map.Map WindowId (Some Window)
  ,failedTransactions :: Set.Set (WindowId,TransactionId)
  ,holdingSlot :: Short
  ,playerPosition :: (Double,Double,Double)
  ,viewAngle :: (Float,Float)
  ,gameMode :: Gamemode
  -- player inventory is window 0
  ,playerInventory :: Inventory
  ,diggingBlocks :: Map.Map BlockCoord BlockBreak
  ,moveMode :: MoveMode
  ,playerState :: ServerState
  ,clientUsername :: String
  ,clientBrand :: String
  ,clientUUID :: UUID
  ,playerId :: PlayerId
  ,packetQueue :: TQueue (ForAny (DescribedPacket PacketSerializer))
  }

-- This is a natural transformation from modifying a player to modifying a specific player in the world, given the player id of that player.
modifyPlayerInWorld :: Members '[WorldManipulation,Logging] r => PlayerId -> Eff (State PlayerData ': r) a -> Eff r a
modifyPlayerInWorld pId = generalizedRunNat $ \case
  Get -> _
  Put p' -> _

type CanHandlePackets r = Members '[Packeting,IO,Logging] r

-- Reading the configuration is an effect
type Configured = Reader Configuration

data PacketHandler p = PacketHandler 
  {packetThreadingMode :: ThreadingMode
  ,onPacket :: forall r. CanHandlePackets r => p -> Eff r ()
  ,deserializePacket :: forall m. MonadGet m => m p
  }

type InboundPacketDescriptor = PacketDescriptor PacketHandler

data Configuration = Configuration
  {shouldLog :: LogLevel -> Bool
  ,protocolVersion :: Integer
  ,serverVersion :: Text
  ,serverName :: Text
  ,serverMotd :: Text
  ,spawnLocation :: BlockCoord
  ,defaultDifficulty :: Difficulty
  ,defaultDimension :: Dimension
  ,defaultGamemode :: Gamemode
  ,maxPlayers :: Word8
  ,compressionThreshold :: Maybe VarInt
  ,packetsForState :: ServerState -> SupportedPackets PacketHandler
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  {shouldLog = \case {HexDump -> False; _ -> True}
  ,protocolVersion = 0
  ,serverVersion = "Default"
  ,serverName = "Civskell"
  ,serverMotd = "An Experimental Minecraft server written in Haskell | github.com/Lazersmoke/civskell"
  ,spawnLocation = BlockLocation (0,64,0)
  ,defaultDifficulty = Peaceful
  ,defaultDimension = Overworld
  ,defaultGamemode = Survival
  -- TODO: Check against this
  ,maxPlayers = 100
  ,compressionThreshold = Just 16
  ,packetsForState = const Vector.empty
  }

newtype Slot = Slot (Maybe SlotData)

instance Show Slot where
  show (Slot (Just dat)) = "{" ++ show dat ++ "}"
  show (Slot Nothing) = "{}"

data SlotData = SlotData (Some Item) Word8

class Item i where
  -- TODO: Type level this when we have dependent types
  itemId :: Short
  itemIdentifier :: String
  itemMeta :: i -> Short
  itemMeta _ = 0
  --itemPlaced :: i -> Maybe (Some Block)
  --itemPlaced _ = Nothing
  itemNBT :: i -> Maybe NbtContents
  itemNBT _ = Nothing
  -- TODO: param Slot i
  parseItem :: Parser Slot
  -- Some items do something when right clicked
  onItemUse :: forall r. Member Logging r => Maybe (i -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Eff r ())
  onItemUse = Nothing

-- An inventory is a mapping from Slot numbers to Slot data (or lack thereof).
-- TODO: Reconsider sparsity obligations of Map here, and decide on a better
-- internal representation. Potentially make this abstract after all. There
-- might be a really fitting purely functional data structure to show off here.
type Inventory = Map.Map Short Slot

-- Abstraction methods for Inventory, not really needed tbh.
getSlot :: Short -> Inventory -> Slot
getSlot = Map.findWithDefault (Slot Nothing)

setSlot :: Short -> Slot -> Inventory -> Inventory
setSlot i (Slot Nothing) = Map.delete i
setSlot i s = Map.insert i s

-- Sidestep existential nonsense when creating items
slot :: Item i => i -> Word8 -> Slot
slot i c = Slot . Just $ SlotData (ambiguate . Identity $ i) c

-- This is a rather sketchy instance because itemId uniqueness is not enforced
-- at type level, so we could actually get two different Item instances claiming
-- the same id, meta, and NBT, which would make this `Eq` instance unreliable.
-- In fact, it might be *impossible* to write an `Eq` instance for Slots
-- involving general, open typeclass `Item`s because that would involve deciding
-- equality for a `forall a. c a => p a` sort of thing, which would only be
-- based on the `c` instance.
instance Eq SlotData where
  (SlotData (SuchThat (Identity (a :: at))) ac) == (SlotData (SuchThat (Identity (b :: bt))) bc) = itemId @at == itemId @bt && itemMeta a == itemMeta b && itemNBT a == itemNBT b && ac == bc

slotAmount :: Slot -> Word8
slotAmount (Slot (Just (SlotData _ c))) = c
slotAmount (Slot Nothing) = 0

-- Remove as many items as possible, up to count, return amount removed
removeCount :: Word8 -> Slot -> (Word8,Slot)
removeCount _ (Slot Nothing) = (0,Slot Nothing)
removeCount c (Slot (Just (SlotData i count))) = if c < count then (c,Slot . Just $ SlotData i (count - c)) else (count,Slot Nothing)

-- Count is how many to put into first stack from second
splitStack :: Word8 -> Slot -> (Slot,Slot)
splitStack _ (Slot Nothing) = (Slot Nothing,Slot Nothing)
splitStack c s@(Slot (Just (SlotData i _))) = (firstStack,secondStack)
  where
    firstStack = if amtFirstStack == 0 then Slot Nothing else Slot . Just $ SlotData i amtFirstStack
    (amtFirstStack,secondStack) = removeCount c s

-- Pretty print a slot's data. Perhaps we should let `Item`s provide a fancy name for this instance to use.
instance Show SlotData where
  show (SlotData (SuchThat (Identity (i :: it))) count) = show count ++ " of [" ++ show (itemId @it) ++ ":" ++ show (itemMeta i) ++ "]" ++ n (itemNBT i)
    where
      -- Only display the NBT if it is actually there
      n Nothing = ""
      n (Just nbt) = " with tags: " ++ show nbt

instance Serial Slot where
  serialize (Slot Nothing) = serialize (-1 :: Short)
  serialize (Slot (Just sd)) = serialize @SlotData sd
  deserialize = deserialize @Short >>= \case
    (-1) -> pure $ Slot Nothing
    _itemId -> error "Unimplemented: Deserialization of general Slots. May be unimplementable in general :/"

instance Serial SlotData where
  serialize (SlotData (SuchThat (Identity (i :: it))) count) = serialize (itemId @it) >> serialize count >> serialize (itemMeta i) >> (case itemNBT i of {Just nbt -> putByteString $ Cereal.encode (NBT "" nbt);Nothing -> putWord8 0x00})
  deserialize = error "Unimplemented: deserialize @SlotData"


generalizedRunNat :: (forall x. f x -> Eff r x) -> Eff (f ': r) a -> Eff r a
generalizedRunNat n = handleRelay pure (\e k -> n e >>= k)

-- logToConsole is a natural transformation from `Writer`ing log messages to logging them to a TQueue for later transactional printing
logToConsole :: Members '[Configured,IO] r => LogQueue -> Eff (Logging ': r) a -> Eff r a
logToConsole (LogQueue l) = generalizedRunNat $ \case
  Writer (LogMessage level str) -> do
    -- Apply the configured loggin predicate to see if this message should be logged
    p <- ($ level) . shouldLog <$> ask 
    sName <- serverName <$> ask
    -- Select the prefix, then send off the log message
    when p $ send . atomically . writeTQueue l . (<>str) $ case level of
      HexDump -> ""
      ClientboundPacket -> "[\x1b[32mSent\x1b[0m] "
      ServerboundPacket -> "[\x1b[32mRecv\x1b[0m] "
      ErrorLog -> "[\x1b[31m\x1b[1mError\x1b[0m] "
      VerboseLog -> "[\x1b[36m" <> sName <> "/Verbose\x1b[0m] "
      (TaggedLog tag) -> "[\x1b[36m" <> tag <> "\x1b[0m] "
      NormalLog -> "[\x1b[36m" <> sName <> "\x1b[0m] "

newtype ProtocolNBT = ProtocolNBT {unProtocolNBT :: NBT}

instance Serial ProtocolNBT where
  serialize = putByteString . Cereal.runPut . Cereal.put . unProtocolNBT
  deserialize = remaining >>= \r -> getByteString (fromIntegral r) >>= \bs -> case Cereal.runGet Cereal.get bs of
    Left e -> error e
    Right a -> pure (ProtocolNBT a)

-- Legacy Handshake packet constant. This is what we send when we get a legacy
-- ping from a client.
legacyHandshakePongConstant :: BS.ByteString
legacyHandshakePongConstant = BS.pack [0xFF,0x00,0x1b,0x00,0xa7
  ,0x00,0x31 -- 1
  ,0x00,0x00 -- Seperator
  ,0x00,0x33 -- 3 -- Protocol Version number. TODO: Update this with protocolVersion
  ,0x00,0x33 -- 3
  ,0x00,0x35 -- 5
  ,0x00,0x00 -- Seperator
  ,0x00,0x31 -- 1 -- MC Version
  ,0x00,0x2e -- .
  ,0x00,0x31 -- 1
  ,0x00,0x31 -- 1
  ,0x00,0x2e -- .
  ,0x00,0x32 -- 2
  ,0x00,0x00 -- Seperator
  ,0x00,0x43 -- C -- Server Name
  ,0x00,0x69 -- i
  ,0x00,0x76 -- v
  ,0x00,0x73 -- s
  ,0x00,0x6b -- k
  ,0x00,0x65 -- e
  ,0x00,0x6c -- l
  ,0x00,0x6c -- l
  ,0x00,0x00 -- Seperator
  ,0x00,0x00 -- 0
  ,0x00,0x00 -- Seperator
  ,0x00,0x00 -- 0
  ]

-- First part of the legacy client ping packet
legacyHandshakePingConstant :: BS.ByteString
legacyHandshakePingConstant = BS.pack
  [0xFE -- SLP PktId
  ,0x01 -- SLP "payload"
  ,0xFA -- Plugin Message
  ,0x00,0x0B -- Length of "MC|PingHost"
  ,0x00,0x4D -- M
  ,0x00,0x43 -- C
  ,0x00,0x7C -- |
  ,0x00,0x50 -- P
  ,0x00,0x69 -- i
  ,0x00,0x6E -- n
  ,0x00,0x67 -- g
  ,0x00,0x48 -- H
  ,0x00,0x6F -- o
  ,0x00,0x73 -- s
  ,0x00,0x74 -- t
  ]

-----------------
-- Named Types --
-----------------


-- A Nibble is a Word8 that we promise (not enforced!!!) to only use the 4 least
-- significant bits.
-- TODO: Investigate `(Bool,Bool,Bool,Bool)` or `Vect 4 Bool` as alternative.
--type Nibble = Word8

-- An EncrptionCouplet holds the current internal state of the encryption
-- mechanism, including both directions' shift buffers. (Cipher,Enc,Dec)
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- Verbify this to match all our other type synonyms like `HasWorld`
type PerformsIO r = Member IO r

-- Auth infrastructure is scary, I hope it doesn't break ever :3
-- Parsed form of the result of authenticating a player with Mojang
data AuthPacket = AuthPacket UUID String [AuthProperty]

-- FromJSON instances parse JSON into AuthPackets
instance Data.Aeson.Types.FromJSON AuthPacket where
  parseJSON (Aeson.Object o) = AuthPacket <$> o .: "id" <*> o .: "name" <*> o .: "properties"
  parseJSON x = Data.Aeson.Types.typeMismatch "AuthPacket" x
-- Helper type to support the arbitrary properties in auth packets
data AuthProperty = AuthProperty ProtocolString ProtocolString (ProtocolOptional ProtocolString) deriving Generic

instance Show AuthProperty where
  show (AuthProperty (ProtocolString name) (ProtocolString value) (ProtocolOptional _mSig)) = "\"" ++ name ++ "\":\"" ++ value ++ "\""

instance Serial AuthProperty where {}

instance Data.Aeson.Types.FromJSON AuthProperty where
  parseJSON = withObject "Auth Property" $ \o -> AuthProperty <$> o .: "name" <*> o .: "value" <*> (ProtocolOptional <$> o .:? "signature")

data ClientAuthResponse = ClientAuthResponse {accessToken :: String, clientToken :: String, profileInformation :: (Maybe [ClientAuthProfile],Maybe ClientAuthProfile)} deriving Show

instance Data.Aeson.Types.FromJSON ClientAuthResponse where
  parseJSON = withObject "Client Auth Response" $ \o -> ClientAuthResponse <$> o .: "accessToken" <*> o .: "clientToken" <*> ((,) <$> o .:? "availableProfiles" <*> o .:? "selectedProfile")

data ClientAuthProfile = ClientAuthProfile {clientAuthProfileId :: String, clientAuthProfileName :: String, clientAuthProfileIsLegacy :: Bool} deriving Show

instance Data.Aeson.Types.FromJSON ClientAuthProfile where
  parseJSON = withObject "Client Auth Profile" $ \o -> ClientAuthProfile <$> o .: "id" <*> o .: "name" <*> o .:? "legacy" .!= False

-------------------------
-- Packet Helper Types --
-------------------------
-- Should these be moved next to their respective packet definitions in
-- Civskell.Packet.*bound?

-- Used in Client.Animation; Server.Animation just uses hand
data AnimationAction = SwingHand Hand | Critical Bool | TakeDamage | LeaveBedAnimation deriving Show

instance Serial AnimationAction where
  serialize x = putWord8 $ case x of
    (SwingHand MainHand) -> 0x00
    TakeDamage -> 0x01
    LeaveBedAnimation -> 0x02
    (SwingHand OffHand) -> 0x03
    (Critical False) -> 0x04
    (Critical True) -> 0x05
  deserialize = getWord8 >>= pure . \case
    0x00 -> SwingHand MainHand
    0x01 -> TakeDamage
    0x02 -> LeaveBedAnimation
    0x03 -> SwingHand OffHand
    0x04 -> Critical False
    0x05 -> Critical True
    _ -> error "Bad Deserialize on AnimationAction"

-- Used in Server.UseEntity
data EntityInteraction = Attack | Interact Hand | InteractAt (Float,Float,Float) Hand deriving Show

instance Serial EntityInteraction where
  serialize = \case
    Interact hand -> serialize @VarInt 0 *> serialize @Hand hand
    Attack -> serialize @VarInt 1
    InteractAt loc hand -> serialize @VarInt 2 *> serialize loc *> serialize @Hand hand
  deserialize = deserialize @VarInt >>= \case
    0 -> Interact <$> deserialize @Hand
    1 -> pure Attack
    2 -> InteractAt <$> deserialize @(Float,Float,Float) <*> deserialize @Hand
    x -> error $ "deserialize @EntityInteraction: Got (VarInt) " <> show x

data AsType = AsBlock | AsItem
-- Used in Server.PlayerDigging
data PlayerDigAction = StartDig BlockCoord BlockFace | StopDig BlockCoord BlockFace | EndDig BlockCoord BlockFace | DropItem Bool | ShootArrowOrFinishEating | SwapHands

instance Serial PlayerDigAction where
  serialize = \case
    StartDig bc bf -> serialize @VarInt 0 *> serialize bc *> serialize bf
    StopDig bc bf -> serialize @VarInt 1 *> serialize bc *> serialize bf
    EndDig bc bf -> serialize @VarInt 2 *> serialize bc *> serialize bf
    DropItem True -> serialize @VarInt 3
    DropItem False -> serialize @VarInt 4
    ShootArrowOrFinishEating -> serialize @VarInt 5
    SwapHands -> serialize @VarInt 6
  deserialize = deserialize @VarInt >>= \case
    0 -> StartDig <$> deserialize @BlockCoord <*> deserialize @BlockFace
    1 -> StopDig <$> deserialize @BlockCoord <*> deserialize @BlockFace
    2 -> EndDig <$> deserialize @BlockCoord <*> deserialize @BlockFace
    3 -> pure (DropItem True)
    4 -> pure (DropItem False)
    5 -> pure ShootArrowOrFinishEating
    6 -> pure SwapHands
    x -> error $ "deserialize @PlayerDigAction: Got (VarInt) " <> show x

-- Used in Server.EntityAction
data PlayerEntityAction = Sneak Bool | Sprint Bool | HorseJumpStart VarInt | HorseJumpStop | LeaveBed | ElytraFly | HorseInventory

instance Serial PlayerEntityAction where
  serialize = \case
    Sneak True -> serialize @VarInt 0
    Sneak False -> serialize @VarInt 1
    LeaveBed -> serialize @VarInt 2
    Sprint True -> serialize @VarInt 3
    Sprint False -> serialize @VarInt 4
    HorseJumpStart jmp -> serialize @VarInt 5 >> serialize @VarInt jmp
    HorseJumpStop -> serialize @VarInt 6
    HorseInventory -> serialize @VarInt 7
    ElytraFly -> serialize @VarInt 8
  deserialize = deserialize @VarInt >>= \case
    0 -> pure (Sneak True)
    1 -> pure (Sneak False)
    2 -> pure LeaveBed
    3 -> pure (Sprint True)
    4 -> pure (Sprint False)
    5 -> HorseJumpStart <$> deserialize @VarInt
    6 -> pure HorseJumpStop
    7 -> pure HorseInventory
    8 -> pure ElytraFly
    x -> error $ "deserialize @PlayerEntityAction: Got (VarInt) " <> show x

-- Used in Server.ClickWindow
data InventoryClickMode = NormalClick Bool | ShiftClick Bool | NumberKey Word8 | MiddleClick | ItemDropOut Bool | PaintingMode Word8 | DoubleClick deriving Show

-- Used in Server. and Client.PlayerAbilities
-- Invuln, Flying, Allow Flying, Creative
data AbilityFlags = AbilityFlags Bool Bool Bool Bool

instance Serial AbilityFlags where
  serialize (AbilityFlags i f af c) = serialize $ u i 0 .|. u f 1 .|. u af 2 .|. u c 3
    where
      u b = shiftL (unsafeCoerce b :: Word8)
  deserialize = (\w -> AbilityFlags (testBit w 0) (testBit w 1) (testBit w 2) (testBit w 3)) <$> getWord8

-- Used in Client.ChangeGameState
data GameStateChange = InvalidBed | Raining Bool | ChangeGamemode Gamemode | ExitTheEnd Bool | DemoMessage | ArrowHitOtherPlayer | FadeValue Float | FadeTime Float | ElderGuardian

-- Used in Server.ClientAction
data ClientStatusAction = PerformRespawn | RequestStats | OpenInventory deriving (Show,Enum)

instance Serial ClientStatusAction where
  serialize = serialize @VarInt . fromIntegral . fromEnum
  deserialize = toEnum . fromIntegral <$> (deserialize @VarInt)

-- Block Coord helper functions
-- Get the chunk an absolute coord is in
blockToChunk :: BlockCoord -> ChunkCoord
blockToChunk (BlockLocation (x,y,z)) = ChunkCoord (x `div` 16,y `div` 16,z `div` 16)

-- Get the relative location inside its chunk of an absolute block coord
blockToRelative :: BlockCoord -> BlockOffset
blockToRelative (BlockLocation (x,y,z)) = BlockLocation (f x,f y,f z)
  where
    -- We don't use negative relative coords in negative chunks
    f n = if mod n 16 < 0 then 16 + mod n 16 else mod n 16
-- Get the block coord adjacent to a given coord on a given side
blockOnSide :: BlockCoord -> BlockFace -> BlockCoord
blockOnSide (BlockLocation (x,y,z)) Bottom = BlockLocation (x,y-1,z)
blockOnSide (BlockLocation (x,y,z)) Top = BlockLocation (x,y+1,z)
blockOnSide (BlockLocation (x,y,z)) (SideFace North) = BlockLocation (x,y,z-1)
blockOnSide (BlockLocation (x,y,z)) (SideFace South) = BlockLocation (x,y,z+1)
blockOnSide (BlockLocation (x,y,z)) (SideFace West) = BlockLocation (x-1,y,z)
blockOnSide (BlockLocation (x,y,z)) (SideFace East) = BlockLocation (x+1,y,z)

--some :: c a => a -> Some c
--some = ambiguate . Identity

-- The state of a block in the world (Block id, damage)
data BlockState = BlockState Short Word8 deriving Eq

-- Used in PlayerData to track the breaking stage of a block the player is mining.
data BlockBreak 
  = InProgress Word8 -- The block is still being broken, and has been for this many ticks.
  | DoneBreaking -- The block is done breaking and should be removed, and this entry removed.

-- Combinator for serializing Maybes in a Minecrafty way
--serOpt :: (MonadPut m,Serial s) => Maybe s -> m ()
--serOpt (Just a) = serialize True >> serialize a
--serOpt Nothing = serialize False

--deserOpt :: forall s m. (MonadGet m,Serial s) => m (Maybe s)
--deserOpt = deserialize @Bool >>= \case
 -- False -> pure Nothing
  --True -> Just <$> deserialize @s

newtype ProtocolOptional a = ProtocolOptional {unProtocolOptional :: Maybe a} deriving (Eq,Show)

instance Serial a => Serial (ProtocolOptional a) where
  serialize (ProtocolOptional ma) = case ma of
    Just a -> serialize True *> serialize a
    Nothing -> serialize False
  deserialize = deserialize @Bool >>= \case
    False -> pure (ProtocolOptional Nothing)
    True -> ProtocolOptional . Just <$> deserialize @a

newtype ProtocolString = ProtocolString {unProtocolString :: String} deriving (IsString,Eq,Data.Aeson.Types.FromJSON)

instance Show ProtocolString where
  show (ProtocolString s) = show s

instance Serial ProtocolString where
  serialize (ProtocolString str) = serialize ((fromIntegral $ BS.length encoded) :: VarInt) >> putByteString encoded
    where
      encoded = Data.ByteString.UTF8.fromString str
  deserialize = deserialize @VarInt >>= \len -> ProtocolString . Data.ByteString.UTF8.toString <$> getBytes (fromIntegral len)

data ProtocolList i a = ProtocolList {unProtocolList :: [a]}

instance Exts.IsList (ProtocolList i a) where
  type Item (ProtocolList i a) = a
  fromList = ProtocolList
  toList = unProtocolList

instance (Integral i,Serial i,Serial a) => Serial (ProtocolList i a) where
  serialize (ProtocolList xs) = serialize ((fromIntegral $ length xs) :: i) >> traverse serialize xs >> pure ()
  deserialize = deserialize @i >>= \len -> ProtocolList <$> replicateM (fromIntegral len) deserialize

-- TODO: ensure the generics instance is correct here
--instance Serial Bool where
  --serialize True = BS.singleton 0x01
  --serialize False = BS.singleton 0x00

-- Annotate a BS with its length as a VarInt
withLength :: MonadPut m => BS.ByteString -> m ()
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) >> putByteString bs

-- Annotate a list of serializable things with the length of the list
withListLength :: (MonadPut m,Serial s) => [s] -> m ()
withListLength ls = serialize ((fromIntegral $ length ls) :: VarInt) >> traverse_ serialize ls

newtype LengthAnnotatedByteString = LengthAnnotatedByteString {unLengthAnnotatedByteString :: BS.ByteString} deriving (Show,Eq)

instance Serial LengthAnnotatedByteString where
  serialize (LengthAnnotatedByteString bs) = serialize @VarInt (fromIntegral $ BS.length bs) *> putByteString bs
  deserialize = LengthAnnotatedByteString <$> (getByteString . fromIntegral =<< deserialize @VarInt)

-- Entities are things with properties
class Entity m where
  entityName :: Text
  entityType :: VarInt
  entitySize :: m -> (Float,Float,Float)
  entityLocation :: m -> EntityLocation
  entityVelocity :: m -> EntityVelocity
  entityMeta :: m -> [EntityMetadata]

class Entity m => Mob m where {}

-- Some entities are Objects, which can haev special, different properties
class Entity m => Object m where
  objectName :: Text
  objectId :: Word8
  -- TODO: does objectSize always == entitySize? If so, remove objectSize
  objectSize :: m -> (Float,Float,Float)
  objectSize = entitySize
  -- TODO: does objectLocation always == entityLocation? If so, remove objectLocation
  objectLocation :: m -> EntityLocation
  objectLocation = entityLocation
  objectData :: m -> (Int32,Maybe EntityVelocity)

{-
defaultObject :: Entity m -> String -> Word8 -> (m -> (Int32,Maybe EntityVelocity)) -> Object m
defaultObject e name oId oDat = MkObject
  {entityData = e
  ,objectName = name
  ,objectId = oId
  ,objectSize = entitySize e
  ,objectLocation = entityLocation e
  ,objectData = oDat
  }
-}

-- EntityMeta is just a normal value with special type properties
newtype EntityMeta p = EntityMeta p deriving Generic

-- Those special properties are that the type has an instance for `EntityMetaType`
type EntityMetadata = SuchThat '[EntityMetaType] EntityMeta

newtype EntityPropertySet = EntityPropertySet [Maybe EntityMetadata]

-- Helper data structures to reduce reptition
data EntityLocation = EntityLocation (Double,Double,Double) (Word8,Word8) 
data EntityVelocity = EntityVelocity (Short,Short,Short)

-- EntityMeta can be Seriald in a special way, involving the `entityMetaFlag`
class Serial (EntityMeta p) => EntityMetaType p where entityMetaFlag :: Word8

-- Serializing a bunch of EntityMetadatas involves indexing, so Nothing means that is absent, but it still increments the index counter
instance Serial EntityPropertySet where
  serialize (EntityPropertySet list) = go 0 list
    where
      go :: MonadPut m => Word8 -> [Maybe EntityMetadata] -> m ()
      go ix (Just (SuchThat (EntityMeta (m::p))):xs) = putWord8 ix >> serialize (entityMetaFlag @p) >> serialize (EntityMeta m) >> go (succ ix) xs
      go ix (Nothing : xs) = go (succ ix) xs
      go _ [] = putWord8 0xff
  deserialize = error "Undefined: EntityPropertySet deserialize"

-- NYI: (4,Chat) (7,Rotation 3xFloat) (8,Position)

instance EntityMetaType Word8 where entityMetaFlag = 0x00
instance Serial (EntityMeta Word8) where {}

instance EntityMetaType VarInt where entityMetaFlag = 0x01
instance Serial (EntityMeta VarInt) where {}

instance EntityMetaType Float where entityMetaFlag = 0x02
instance Serial (EntityMeta Float) where {}

instance EntityMetaType String where entityMetaFlag = 0x03
instance Serial (EntityMeta String) where {}

instance EntityMetaType SlotData where entityMetaFlag = 0x05
instance Serial (EntityMeta SlotData) where {}

instance EntityMetaType Bool where entityMetaFlag = 0x06
instance Serial (EntityMeta Bool) where {}

instance EntityMetaType (ProtocolOptional BlockCoord) where entityMetaFlag = 0x09
instance Serial (EntityMeta (ProtocolOptional BlockCoord)) where {}

instance EntityMetaType BlockFace where entityMetaFlag = 0x0A
instance Serial (EntityMeta BlockFace) where 
  serialize (EntityMeta f) = serialize @VarInt $ fromIntegral (fromEnum f)
  deserialize = EntityMeta . toEnum . fromIntegral <$> deserialize @VarInt

instance EntityMetaType (ProtocolOptional UUID) where entityMetaFlag = 0x0B
instance Serial (EntityMeta (ProtocolOptional UUID)) where {}

instance EntityMetaType (Maybe BlockState) where entityMetaFlag = 0x0C
instance Serial (EntityMeta (Maybe BlockState)) where 
  serialize (EntityMeta m) = serialize @VarInt $ case m of {Nothing -> 0;Just (BlockState bid dmg) -> unsafeCoerce $ shiftL bid 4 .|. unsafeCoerce dmg}
  deserialize = deserialize @VarInt >>= \case
    0 -> pure (EntityMeta Nothing)
    x -> pure $ EntityMeta $ Just $ BlockState (unsafeCoerce $ shiftR x 4) (unsafeCoerce $ x .&. 0xf)

-- Indent the hexdump; helper function
indentedHex :: BS.ByteString -> String
indentedHex = init . unlines . map ("  "++) . lines . prettyHex

comb :: Some c -> (forall a. c a => r) -> r
comb (SuchThat (Identity (_ :: a))) f = f @a
