{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Civskell.Data.Types
  -- Constants
  (airChunk
  ,allCoords
  ,protocolVersion
  -- Helper functions
  ,blockInChunk
  ,blockToChunk
  ,blockToRelative
  ,blockOnSide
  ,showPacket
  ,jsonyText
  ,withLength
  ,withListLength
  -- Type synonyms
  ,EncryptionCouplet
  ,PerformsIO
  ,Short
  ,UUID(..)
  -- Blocks
  ,BlockBreak(..)
  ,Block(..)
  ,BlockCoord
  ,BlockOffset
  ,BlockFace(..)
  ,BlockState(..)
  -- Chunks
  ,ChunkCoord(..)
  ,ChunkSection(..)
  -- Packet Enums
  ,AnimationAction(..)
  ,PlayerListAction(..)
  ,PlayerListActionType(..)
  ,EntityInteraction(..)
  ,ClientStatusAction(..)
  ,GameStateChange(..)
  ,InventoryClickMode(..)
  ,PlayerDigAction(..)
  ,PlayerEntityAction(..)
  ,AuthPacket(..)
  ,AuthProperty(..)
  -- Id's and newtypes
  ,VarInt(..)
  ,EntityId(..)
  --,PacketId(..) -- Actually a class
  ,WindowId(..)
  ,KeepAliveId
  ,PlayerId
  ,TPConfirmId
  ,TransactionId
  -- Aux Data Types
  ,PlayerData(..)
  ,Slot(..)
  ,slotAmount
  ,removeCount
  ,splitStack
  -- Aux Enums
  ,Difficulty(..)
  ,Gamemode(..)
  ,Hand(..)
  ,MoveMode(..)
  ,AbilityFlags(..)
  ,ServerState(..)
  ,Side(..)
  -- Player
  ,HasPlayer
  ,Player(..)
  -- World
  ,HasWorld
  ,WorldData(..)
  ,World(..)
  -- Logging
  ,Logs
  ,Logging(..)
  ,LogLevel(..)
  -- Network
  ,HasNetworking
  ,Networking(..)
  -- Packet
  ,Packet(..)
  ,ClientPacket(..)
  ,ServerPacket(..)
  -- Entity
  ,Entity(..)
  ,EntityMetaType(..)
  ,EntityMeta(..)
  ,EntityMetadata
  ,EntityLocation(..)
  ,EntityVelocity(EntityVelocity)
  ,Mob
  ,Object(..)
  ,Serialize(..)
  ) where

import Control.Eff
import Control.Concurrent.STM
import Crypto.Cipher.AES (AES128)
import Data.SuchThat
import qualified Data.Text as Text
import Numeric (readHex)
import Data.Aeson hiding (Object)
import Data.Aeson.Types hiding (Parser,Object)
import qualified Data.Aeson
import Data.Bits (Bits)
import Data.Int (Int16,Int32,Int64)
import Data.List (intercalate)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import Data.NBT
import Data.Word (Word64,Word8)
import Data.Semigroup ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import qualified Data.Serialize as Ser
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary.BitBuilder as BB
import qualified Data.Map.Lazy as Map
import Data.Attoparsec.ByteString (Parser)
import Unsafe.Coerce (unsafeCoerce)
import Data.Bits
import Numeric (showHex)
import Data.List (unfoldr,foldl')
import qualified Data.Set as Set

protocolVersion :: Integer
protocolVersion = 316

-- Type Synonyms
type Short = Int16
-- Cipher, [Enc|Dec]
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)
-- `PerformsIO` effect
type PerformsIO r = Member IO r
-- You make an effect that `PerformsIO` by `send`ing an IO action (`send (putStrLn "meow")`)
newtype UUID = UUID (Word64,Word64)

instance Show UUID where
  show (UUID (ua,ub)) = reformat $ showHex ua (showHex ub "")
    where
      -- Add a hyphen at an index
      ins i s = let (a,b) = splitAt i s in a ++ "-" ++ b
      -- Reformat the uuid to what the client expects
      reformat = ins 8 . ins 12 . ins 16 . ins 20

instance Serialize UUID where
  serialize (UUID (a,b)) = serialize a <> serialize b

-- Simple way to inject a text message into a json chat string
jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"

-- Datatypes
-- Used in PlayerInfo to track the breaking stage of a block the player is mining
data BlockBreak = InProgress Word8 | DoneBreaking

-- A block coordinate, not an entity position. Ord is derivied for use in maps.
data Block (r :: Relativity) = Block (Int,Int,Int) deriving (Eq,Ord)
type BlockCoord = Block 'Absolute
type BlockOffset = Block 'Relative

instance Show BlockCoord where
  show (Block (x,y,z)) = "(Block)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

instance Show BlockOffset where
  show (Block (x,y,z)) = "(Offset)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

{- This is a bad instance that took almost an hour to debug. It still isn't fixed, just worked around (it's derived, and we don't rely on its ordering in Map.elems)
instance Ord BlockCoord where
  compare a@(BlockCoord (xa,ya,za)) b@(BlockCoord (xb,yb,zb)) = if a == b then EQ else if yb > ya then GT else if zb > za then GT else if xb > xa then GT else LT
-}

instance Serialize BlockCoord where
  serialize (Block (x,y,z)) = serialize $
    (shiftL (u x .&. 0x3FFFFFF) 38) .|.
    (shiftL (u y .&. 0xFFF) 26) .|.
    (u z .&. 0x3FFFFFF)
    where
      u = unsafeCoerce :: Int -> Int64

newtype ChunkCoord = ChunkCoord (Int,Int,Int) deriving (Eq,Ord)

instance Show ChunkCoord where
  show (ChunkCoord (x,y,z)) = "(Chunk)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

-- VarInt's max size is an Int32, so we use that to store it. Note that they are also serialized different than a normal MCInt
newtype VarInt = VarInt {unVarInt :: Int32} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show VarInt where show = show . unVarInt

instance Serialize VarInt where
  serialize n = if moreAfter
    -- If there are more, set the msb and recurse
    then (0b10000000 .|. writeNow) `BS.cons` serialize (shiftR n 7)
    -- Otherwise, just use this one
    else BS.singleton writeNow
    where
      -- Write first seven bits
      writeNow = (unsafeCoerce :: VarInt -> Word8) $ n .&. 0b1111111
      -- Are there more bytes to add?
      moreAfter = shiftR n 7 /= 0

newtype EntityId = EntityId {unEID :: VarInt} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show EntityId where show = (\p -> "Entity Id {" ++ p ++ "}") . show . unEID
instance Serialize EntityId where serialize = serialize . unEID

newtype WindowId = WindowId {unWID :: Word8} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show WindowId where show = (\p -> "Window Id {" ++ p ++ "}") . show . unWID
instance Serialize WindowId where serialize = serialize . unWID

type PlayerId = EntityId

type TransactionId = Short
type KeepAliveId = VarInt
type TPConfirmId = VarInt

-- Block id, count, damage
data Slot = EmptySlot | Slot Short Word8 Short (Maybe NbtContents) deriving Eq

slotAmount :: Slot -> Word8
slotAmount EmptySlot = 0
slotAmount (Slot _ c _ _) = c

-- Remove as many items as possible, up to count, return amount removed
removeCount :: Word8 -> Slot -> (Word8,Slot)
removeCount _ EmptySlot = (0,EmptySlot)
removeCount c (Slot bid count dmg nbt) = if c < count then (c,Slot bid (count - c) dmg nbt) else (count,EmptySlot)

-- Count is how many to put into first stack from second
splitStack :: Word8 -> Slot -> (Slot,Slot)
splitStack _ EmptySlot = (EmptySlot,EmptySlot)
splitStack c s@(Slot bid _ dmg nbt) = (firstStack,secondStack)
  where
    firstStack = if amtFirstStack == 0 then EmptySlot else Slot bid amtFirstStack dmg nbt
    (amtFirstStack,secondStack) = removeCount c s

instance Show Slot where
  show EmptySlot = "{}"
  show (Slot bid count dmg mNbt) = "{" ++ show count ++ " of [" ++ show bid ++ ":" ++ show dmg ++ "]" ++ n mNbt ++ "}"
    where
      -- Only display the NBT if it is actually there
      n Nothing = ""
      n (Just nbt) = " with tags: " ++ show nbt

instance Serialize Slot where
  serialize EmptySlot = serialize (-1 :: Short)
  serialize (Slot bid count dmg (Just nbt)) = serialize bid <> serialize count <> serialize dmg <> serialize (NBT "" nbt)
  serialize (Slot bid count dmg Nothing) = serialize bid <> serialize count <> serialize dmg <> BS.singleton 0x00

-- Maps coords to non-air blocks. BlockState 0 0 doesn't mean anything. Air just doesn't have anything in the map.
data ChunkSection = ChunkSection (Map BlockOffset BlockState)

-- Derived instance is really long
instance Show ChunkSection where
  show _ = "<Chunk Section>"

-- TODO: Add paletteing. Right now, we send a full 13 bits for *every block*
instance Serialize ChunkSection where
  serialize (ChunkSection bs) = serialize bitsPerBlock <> palette <> chunkData <> lights <> lights
    where
      lights = LBS.toStrict $ BB.toLazyByteString $ createLights $ merged

      -- First 9 bits are the block id, last 4 are the damage
      bitsPerBlock = 13 :: VarInt

      -- Send 0x00 as the length of the palette since we aren't using it
      palette = BS.singleton 0x00

      -- Notify and crash *right away* if this gets fucked up. This only happens if we have a bad Ord instance.
      -- Note that Map.size is O(1) so we aren't wasting too much time here
      -- Merge the chunk's blocks with air because air blocks don't exist in the chunk normally
      merged :: Map BlockOffset BlockState
      merged = if Map.size (Map.union bs airChunk) == 4096 then Map.union bs airChunk else error "Bad Chunksection"

      -- `longChunks` is because Mojang likes to twist their data into weird shapes
      sArray :: BS.ByteString
      sArray = LBS.toStrict . longChunks . BB.toLazyByteString $ sBlockStates merged

      -- This should be the final result, but mojang is weird about chunk sections :S
      sBlockStates :: Map BlockOffset BlockState -> BB.BitBuilder
      sBlockStates m = foldl' (\bb bc -> aBlock (Map.findWithDefault (BlockState 0 0) bc m) `BB.append` bb) BB.empty allCoords

      -- Annotate the data with its length in Minecraft Longs (should always be a whole number assuming 16^3 blocks/chunk)
      chunkData :: BS.ByteString
      chunkData = serialize (fromIntegral (BS.length sArray) `div` 8 :: VarInt) <> sArray

      -- Right now we `const 15` the blocks, so all the blocks are fullbright
      -- TODO: Add lighting information somewhere or derive it here
      createLights = Map.foldl' (\bb _ -> BB.fromBits 4 (15 :: Word8) `BB.append` bb) BB.empty

      -- Mojang felt like packing chunks into arrays of longs lol
      longChunks :: LBS.ByteString -> LBS.ByteString
      longChunks unchunked = LBS.concat . reverse $ bsChunksOf 8 unchunked

      -- Helper function for chunking up the BS
      bsChunksOf :: Int64 -> LBS.ByteString -> [LBS.ByteString]
      bsChunksOf x = unfoldr (\a -> if not (LBS.null a) then Just (LBS.splitAt x a) else Nothing)

      -- Serialize a single block: 9 bits bid, 4 bits dmg
      aBlock :: BlockState -> BB.BitBuilder
      aBlock (BlockState bid dmg) = BB.fromBits 9 bid `BB.append` BB.fromBits 4 dmg

data AuthPacket = AuthPacket UUID String [AuthProperty]

instance FromJSON AuthPacket where
  parseJSON (Data.Aeson.Object o) = AuthPacket <$> o .: "id" <*> o .: "name" <*> (o .: "properties")
  parseJSON x = typeMismatch "AuthPacket" x

instance FromJSON UUID where
  parseJSON (String s) = return $ (\i -> UUID (fromInteger $ i .&. 0xFFFFFFFFFFFFFFFF,fromInteger $ shiftR i 8)) . fst . head . readHex . Text.unpack $ s
  parseJSON x = typeMismatch "UUID" x

data AuthProperty = AuthProperty String String (Maybe String)

instance FromJSON AuthProperty where
  parseJSON (Data.Aeson.Object o) = AuthProperty <$> o .: "name" <*> o .: "value" <*> o .:? "signature"
  parseJSON x = typeMismatch "AuthProperty" x

-- Enumerations for the protocol
data Side = Server | Client

-- Kind for BlockCoord
data Relativity = Relative | Absolute

-- These are used for to select the correct parser (packet id 0 is reused in all four modes)
data ServerState = Handshaking | Playing | LoggingIn | Status

-- More gamemodes NYI
data Gamemode = Survival | Creative {- | Adventure | Spectator -} deriving Show

instance Serialize Gamemode where
  serialize Survival = BS.singleton 0x00
  serialize Creative = BS.singleton 0x01
  -- serialize Adventure = BS.singleton 0x02
  -- serialize Spectator = BS.singleton 0x03

data Difficulty = Peaceful | Easy | Normal | Hard deriving Show

instance Serialize Difficulty where
  serialize Peaceful = BS.singleton 0x00
  serialize Easy = BS.singleton 0x01
  serialize Normal = BS.singleton 0x02
  serialize Hard = BS.singleton 0x03

data Hand = MainHand | OffHand deriving (Show,Eq)

-- Used in Client.Animation; Server.Animation just uses hand
data AnimationAction = SwingHand Hand | Critical Bool | TakeDamage | LeaveBedAnimation deriving Show

instance Serialize AnimationAction where
  serialize (SwingHand MainHand) = BS.singleton 0x00
  serialize TakeDamage = BS.singleton 0x01
  serialize LeaveBedAnimation = BS.singleton 0x02
  serialize (SwingHand OffHand) = BS.singleton 0x03
  serialize (Critical False) = BS.singleton 0x04
  serialize (Critical True) = BS.singleton 0x05

data EntityInteraction = Attack | Interact Hand | InteractAt (Float,Float,Float) Hand deriving Show

data PlayerListActionType = AddPlayer | UpdateGamemode | UpdateLatency | UpdateName | RemovePlayer

data PlayerListAction (a :: PlayerListActionType) where
  PlayerListAdd :: String -> [AuthProperty] -> Gamemode -> VarInt -> (Maybe String) -> PlayerListAction 'AddPlayer
  PlayerListGamemode :: Gamemode -> PlayerListAction 'UpdateGamemode
  PlayerListLatency :: VarInt -> PlayerListAction 'UpdateLatency
  PlayerListName :: (Maybe String) -> PlayerListAction 'UpdateName
  PlayerListRemove :: PlayerListAction 'RemovePlayer

instance Serialize (UUID,PlayerListAction a) where
  serialize (u,p) = serialize u <> serialize p

instance Serialize (PlayerListAction a) where
  serialize (PlayerListAdd name props gm ping mDispName) = serialize name <> serialize ((fromIntegral $ length props) :: VarInt) <> BS.concat (map serProp props) <> serialize gm <> serialize ping <> serOpt mDispName
    where
      serProp (AuthProperty n value mSigned) = serialize n <> serialize value <> serOpt mSigned
  serialize (PlayerListGamemode gm) = serialize gm
  serialize (PlayerListLatency ping) = serialize ping
  serialize (PlayerListName mDisp) = serOpt mDisp
  serialize PlayerListRemove = BS.empty

-- Used in Server.PlayerDigging
data PlayerDigAction = StartDig BlockCoord BlockFace | StopDig BlockCoord BlockFace | EndDig BlockCoord BlockFace | DropItem Bool | ShootArrowOrFinishEating | SwapHands

-- Used in Server.EntityAction
data PlayerEntityAction = Sneak Bool | Sprint Bool | HorseJump Bool VarInt | LeaveBed | ElytraFly | HorseInventory

data BlockFace = Bottom | Top | North | South | West | East deriving (Show,Enum)

-- Used in Server.ClickWindow
data InventoryClickMode = NormalClick Bool | ShiftClick Bool | NumberKey Word8 | MiddleClick | ItemDropOut Bool | PaintingMode Word8 | DoubleClick deriving Show

-- Used in PlayerInfo
data MoveMode = Sprinting | Sneaking | Walking | Gliding | Flying

-- Invuln, Flying, Allow Flying, Creative
data AbilityFlags = AbilityFlags Bool Bool Bool Bool

instance Serialize AbilityFlags where
  serialize (AbilityFlags i f af c) = serialize $ u i 0 .|. u f 1 .|. u af 2 .|. u c 3
    where
      u b n = shiftL (unsafeCoerce b :: Word8) n

-- Used in Client.ChangeGameState
data GameStateChange = InvalidBed | Raining Bool | ChangeGamemode Gamemode | ExitTheEnd Bool | DemoMessage | ArrowHitOtherPlayer | FadeValue Float | FadeTime Float | ElderGuardian

data ClientStatusAction = PerformRespawn | RequestStats | OpenInventory deriving Show

blockToChunk :: BlockCoord -> ChunkCoord
blockToChunk (Block (x,y,z)) = ChunkCoord (x `div` 16,y `div` 16,z `div` 16)

blockToRelative :: BlockCoord -> BlockOffset
blockToRelative (Block (x,y,z)) = Block (f x,f y,f z)
  where
    -- We don't use negative relative coords in negative chunks
    f n = if mod n 16 < 0 then 16 + mod n 16 else mod n 16

-- This is just for name sharing. We never have a constraint (PacketId p) => p
--class PacketId p where
  --packetName :: p -> String
  --packetId :: p -> VarInt
  --packetSide :: p -> Side
  --packetState :: p -> ServerState

blockInChunk :: BlockOffset -> ChunkSection -> BlockState
blockInChunk b (ChunkSection m) = fromMaybe (BlockState 0 0) (Map.lookup b m)

-- Get the block coord adjacent to a given coord on a given side
blockOnSide :: BlockCoord -> BlockFace -> BlockCoord
blockOnSide (Block (x,y,z)) Bottom = Block (x,y-1,z)
blockOnSide (Block (x,y,z)) Top = Block (x,y+1,z)
blockOnSide (Block (x,y,z)) North = Block (x,y,z-1)
blockOnSide (Block (x,y,z)) South = Block (x,y,z+1)
blockOnSide (Block (x,y,z)) West = Block (x-1,y,z)
blockOnSide (Block (x,y,z)) East = Block (x+1,y,z)

-- Block id, damage
data BlockState = BlockState Short Word8 deriving Eq

instance Show BlockState where
  show (BlockState bid dmg) = "Block [" ++ show bid ++ ":" ++ show dmg ++ "]"

instance Serialize BlockState where
  serialize (BlockState bid dmg) = serialize $ shiftL (u bid) 4 .|. (v dmg .&. 0x0f)
    where
      u = unsafeCoerce :: Short -> VarInt
      v = unsafeCoerce :: Word8 -> VarInt

allCoords :: [BlockOffset]
allCoords = [Block (x,y,z) | y <- [0..15], z <- [0..15], x <- [0..15]]

airChunk :: Map BlockOffset BlockState
airChunk = Map.fromList [(Block (x,y,z),BlockState 0 0) | x <- [0..15], z <- [0..15], y <- [0..15]]

formatPacket :: String -> [(String,String)] -> String
formatPacket n [] = "{" ++ n ++ "}"
formatPacket n xs = flip (++) "]" . (++) ("{" ++ n ++ "} [") . intercalate " | " . map (\(name,val) -> if val == "" then name else name ++ ": " ++ val) $ xs

--data PlayerInfo = PlayerInfo
  --{playerData :: TVar PlayerData
  --}

data PlayerData = PlayerData
  {teleportConfirmationQue :: Set.Set VarInt
  ,nextTid :: VarInt
  ,keepAliveQue :: Set.Set VarInt
  ,nextKid :: VarInt
  ,holdingSlot :: Short
  ,playerPosition :: (Double,Double,Double)
  ,viewAngle :: (Float,Float)
  ,gameMode :: Gamemode
  ,playerInventory :: Map Short Slot
  ,diggingBlocks :: Map BlockCoord BlockBreak
  ,moveMode :: MoveMode
  ,playerState :: ServerState
  ,clientUsername :: String
  ,clientBrand :: String
  ,clientUUID :: UUID
  ,playerId :: PlayerId
  ,packetQueue :: TQueue (ForAny ClientPacket)
  }


-- Client needs to recieve packet ASAP after we send them
type HasPlayer r = Member Player r

data Player a where
  UsingPlayerData :: (TVar PlayerData -> STM a) -> Player a
  RegisterPlayer :: Player PlayerId

type Logs r = Member Logging r

data Logging a where
  LogString :: LogLevel -> String -> Logging ()

data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | VerboseLog | TaggedLog String | NormalLog deriving Eq

-- TODO: Map PlayerId (TVar PlayerInfo)?
data WorldData = WorldData {chunks :: Map ChunkCoord ChunkSection, entities :: Map EntityId (Some Entity), players :: Map PlayerId (TVar PlayerData), nextEID :: EntityId, nextUUID :: UUID}

type HasWorld r = Member World r

data World a where
  GetChunk :: ChunkCoord -> World ChunkSection
  SetBlock :: BlockState -> BlockCoord -> World ()
  SetChunk :: ChunkSection -> ChunkCoord -> World ()
  SetColumn :: [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> World ()
  FreshEID :: World EntityId
  FreshUUID :: World UUID
  SetPlayer :: PlayerId -> PlayerData -> World ()
  GetPlayer :: PlayerId -> World PlayerData
  NewPlayer :: TVar PlayerData -> World PlayerId
  GetEntity :: EntityId -> World (Some Entity)
  DeleteEntity :: EntityId -> World ()
  SummonMob :: (Some Mob) -> World ()
  SummonObject :: (Some Object) -> World ()
  AllPlayers :: World [PlayerData]
  --ForallPlayers :: (PlayerInfo -> PlayerInfo) -> World ()
  BroadcastPacket :: ForAny ClientPacket -> World ()

-- Things that can be serialized into a BS for the network
class Serialize s where
  serialize :: s -> BS.ByteString

serOpt :: Serialize s => Maybe s -> BS.ByteString
serOpt (Just a) = serialize True <> serialize a
serOpt Nothing = serialize False

-- Instances for haskell types
--
instance Serialize NBT where
  serialize = Ser.encode

instance Serialize Word8 where
  serialize = BS.singleton

instance Serialize [Char] where
  serialize str = serialize ((fromIntegral $ BS.length encoded) :: VarInt) `BS.append` encoded
    where
      encoded = Data.ByteString.UTF8.fromString str

instance Serialize Bool where
  serialize True = BS.singleton 0x01
  serialize False = BS.singleton 0x00

instance Serialize Int16 where
  serialize i = BS.pack $ map ((unsafeCoerce :: Int16 -> Word8) . shiftR i) [8,0]

instance Serialize Int32 where
  serialize i = BS.pack $ map ((unsafeCoerce :: Int32 -> Word8) . shiftR i) [24,16..0]

instance Serialize Float where
  serialize = serialize . (unsafeCoerce :: Float -> Int32)

instance Serialize Int64 where
  serialize i = BS.pack $ map ((unsafeCoerce :: Int64 -> Word8) . shiftR i) [56,48..0]

instance Serialize Word64 where
  serialize = serialize . (unsafeCoerce :: Word64 -> Int64)

instance Serialize Double where
  serialize = serialize . (unsafeCoerce :: Double -> Int64)


-- Annotate a BS with its length as a VarInt
withLength :: BS.ByteString -> BS.ByteString
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) <> bs

withListLength :: Serialize s => [s] -> BS.ByteString
withListLength ls = serialize ((fromIntegral $ length ls) :: VarInt) <> BS.concat (map serialize ls)

class Packet p where
  type PacketSide p :: Side
  type PacketState p :: ServerState
  packetPretty :: p -> [(String,String)]
  packetName :: String
  packetId :: VarInt
  -- Spawn a PLT on receipt of each packet
  onPacket :: (HasNetworking r,PerformsIO r,Logs r,HasPlayer r,HasWorld r) => p -> Eff r ()
  onPacket p = send (LogString ErrorLog $ "Unsupported packet: " ++ showPacket p)
  parsePacket :: Parser p

class (Packet p,PacketSide p ~ 'Server,s ~ PacketState p) => SP s p where {}
instance (Packet p,PacketSide p ~ 'Server,s ~ PacketState p) => SP s p where {}

class (Packet p,PacketSide p ~ 'Client,s ~ PacketState p) => CP s p where {}
instance (Packet p,PacketSide p ~ 'Client,s ~ PacketState p) => CP s p where {}

showPacket :: forall p. Packet p => p -> String
showPacket pkt = formatPacket (packetName @p) (packetPretty pkt)

newtype ServerPacket s = ServerPacket (SuchThatStar '[Packet,SP s])

--data ServerPacket s = forall p. (Packet p,PacketSide p ~ 'Server,PacketState p ~ s) => ServerPacket p
--instance Show (ServerPacket s) where show (SuchThatStar p) = showPacket p

--data ForAny (p :: k -> *) = forall s. ForAny (p s)

newtype ClientPacket s = ClientPacket (SuchThatStar '[Packet,CP s,Serialize])
--instance Show (ClientPacket s) where show (ClientPacket p) = showPacket p
--instance Serialize (ClientPacket s) where serialize (ClientPacket p) = serialize p

class Entity m where
  entityName :: String
  entityType :: VarInt
  entitySize :: m -> (Float,Float,Float)
  entityLocation :: m -> EntityLocation
  entityVelocity :: m -> EntityVelocity
  entityMeta :: m -> [EntityMetadata]
  --entityMeta _ = []

class Entity m => Mob m where {}

class Entity m => Object m where
  objectName :: String
  objectId :: Word8
  -- TODO: does objectSize always == entitySize? If so, remove objectSize
  objectSize :: m -> (Float,Float,Float)
  objectSize = entitySize
  -- TODO: does objectLocation always == entityLocation? If so, remove objectLocation
  objectLocation :: m -> EntityLocation
  objectLocation = entityLocation
  objectData :: m -> (Int32,Maybe EntityVelocity)

type EntityMetadata = SuchThat '[EntityMetaType] EntityMeta

data EntityLocation = EntityLocation (Double,Double,Double) (Word8,Word8) 
data EntityVelocity = EntityVelocity (Short,Short,Short)

class Serialize (EntityMeta p) => EntityMetaType p where entityMetaFlag :: Word8

instance Serialize [Maybe EntityMetadata] where
  serialize = go 0
    where
      go :: Word8 -> [Maybe EntityMetadata] -> BS.ByteString
      go ix (Just (SuchThat (EntityMeta (m::p))):xs) = serialize ix <> serialize (entityMetaFlag @p) <> serialize (EntityMeta m) <> go (succ ix) xs
      go ix (Nothing : xs) = go (succ ix) xs
      go _ [] = serialize (0xff :: Word8)

instance EntityMetaType Word8 where entityMetaFlag = 0x00
instance Serialize (EntityMeta Word8) where serialize (EntityMeta a) = serialize a

instance EntityMetaType VarInt where entityMetaFlag = 0x01
instance Serialize (EntityMeta VarInt) where serialize (EntityMeta a) = serialize a

instance EntityMetaType Float where entityMetaFlag = 0x02
instance Serialize (EntityMeta Float) where serialize (EntityMeta a) = serialize a

instance EntityMetaType String where entityMetaFlag = 0x03
instance Serialize (EntityMeta String) where serialize (EntityMeta a) = serialize a

instance EntityMetaType Slot where entityMetaFlag = 0x05
instance Serialize (EntityMeta Slot) where serialize (EntityMeta a) = serialize a

instance EntityMetaType Bool where entityMetaFlag = 0x06
instance Serialize (EntityMeta Bool) where serialize (EntityMeta a) = serialize a

instance EntityMetaType BlockFace where entityMetaFlag = 0xA
instance Serialize (EntityMeta BlockFace) where serialize (EntityMeta f) = serialize @VarInt $ fromIntegral (fromEnum f)

instance EntityMetaType (Maybe BlockCoord) where entityMetaFlag = 0xB
instance Serialize (EntityMeta (Maybe BlockCoord)) where serialize (EntityMeta m) = serOpt m

instance EntityMetaType (Maybe UUID) where entityMetaFlag = 0xB
instance Serialize (EntityMeta (Maybe UUID)) where serialize (EntityMeta m) = serOpt m

instance EntityMetaType (Maybe BlockState) where entityMetaFlag = 0xC
instance Serialize (EntityMeta (Maybe BlockState)) where serialize (EntityMeta m) = serialize @VarInt $ case m of {Nothing -> 0;Just (BlockState bid dmg) -> unsafeCoerce $ shiftL bid 4 .|. unsafeCoerce dmg}

newtype EntityMeta p = EntityMeta p

type HasNetworking r = Member Networking r

data Networking a where
  SetCompressionLevel :: Maybe VarInt -> Networking ()
  AddCompression :: BS.ByteString -> Networking BS.ByteString
  SetupEncryption :: EncryptionCouplet -> Networking ()
  PutIntoNetwork :: BS.ByteString -> Networking ()
  --SetupDecryption :: DecryptionCouplet -> Networking ()
  GetFromNetwork :: Int -> Networking BS.ByteString
  RemoveCompression :: BS.ByteString -> Networking BS.ByteString
  IsPacketReady :: Networking Bool
-- PLTs can send packets, but not recieve them
-- When PLTs perform networking actions, they need to be performed in order w.r.t. eachother, but not the PLT
--
-- PLT dispatcher thread (HasNetworking) can recieve packets, but not send them
