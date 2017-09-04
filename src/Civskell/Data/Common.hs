{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Civskell.Data.Common 
  (module Civskell.Data.Protocol
  ,module Civskell.Data.Common
  ) where

--import Hexdump (prettyHex)
import Data.Semigroup
import Numeric (readHex,showHex)
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Int (Int32,Int64)
import Data.Bits
import Data.Hashable
import Data.Word
import qualified Data.Vector as Vector
import GHC.Generics
import Data.Text (Text)
import Data.SuchThat
import qualified Data.Text as T
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types

import Civskell.Data.Protocol

data PacketDescriptor h p = PacketDescriptor 
  {packetPretty :: p -> [(Text,Text)]
  ,packetName :: Text
  ,packetState :: ServerState
  ,packetHandler :: h p
  }

type OutboundPacketDescriptor = PacketDescriptor PacketSerializer

data DescribedPacket h p = DescribedPacket (PacketDescriptor h p) p

data ThreadingMode = SerialThreading | ParThreading

data PacketSerializer p = PacketSerializer 
  {packetId :: VarInt
  ,serializePacket :: forall m. MonadPut m => p -> m ()
  }

type SupportedPackets h = Vector.Vector (SuchThat '[Serial] (PacketDescriptor h))

moveVec :: Int -> Int -> Vector.Vector a -> Vector.Vector a
moveVec old new v = Vector.concat [before,between,Vector.singleton oldelem,after]
  where
    before = Vector.slice 0 old v
    oldelem = v Vector.! old
    between = Vector.slice (old + 1) (new - old - 1) v
    after = Vector.slice (new + 1) (Vector.length v - new - 1) v 

-- Ignore the function because nobody likes functions
--instance Hashable (SuchThat x (PacketDescriptor h)) where
  --hashWithSalt s (SuchThat desc) = s `hashWithSalt` packetName desc `hashWithSalt` packetId desc `hashWithSalt` packetState desc

--instance Eq (SuchThat x (PacketDescriptor h)) where
  --(SuchThat a) == (SuchThat b) = packetName a == packetName b && packetId a == packetId b && packetState a == packetState b

-- We can't make this an actual `Show` instance without icky extentions. Also,
-- the instance context is not checked during instance selection, so we would
-- have to make a full on `instance Show a where`, which is obviously bad.
showPacket :: PacketDescriptor h p -> p -> Text
showPacket pktDesc p = formatPacket (packetName pktDesc) (packetPretty pktDesc p)

-- Helper function to format Packets during logging
-- Takes a packet name and a list of properties (name value pairs). Empty values only show the name
formatPacket :: Text -> [(Text,Text)] -> Text
formatPacket n [] = "{" <> n <> "}"
formatPacket n xs = flip (<>) "]" . (<>) ("{" <> n <> "} [") . T.intercalate " | " . map (\(name,val) -> if val == "" then name else name <> ": " <> val) $ xs


-- A `InboundPacket s` is a thing that is a HandledPacket with the given PacketState
--newtype InboundPacket s = InboundPacket (SuchThatStar '[PacketWithState s,HandledPacket])

-- A `OutboundPacket s` is a thing with the given PacketState
--newtype OutboundPacket s = OutboundPacket (SuchThatStar '[PacketWithState s,Serial])


-- Packets are ambiguous in the Notchian spec, so we need to carry around a
-- "State" that tells us how to parse the packets. This should eventually be
-- replaced by a config option that contains just a `Set (Parser Packet)` or
-- something like that.
data ServerState = Handshaking | Playing | LoggingIn | Status deriving (Generic,Eq)

instance Hashable ServerState where {}

-----------------------
-- Block Coordinates --
-----------------------

-- A block coordinate, not an entity position. Ord is derivied for use in maps,
-- but does not represent any specific concrete ordering, and should not be
-- relied on for serialization to a given format.
data BlockLocation (r :: Relativity)
  = BlockLocation (Int,Int,Int) -- (x,y,z)
  deriving (Eq,Ord)

-- Kind for BlockOffset vs BlockCoord
data Relativity = Relative | Absolute

-- BlockLocations have some weakish dimensionality. `Absolute` means that it is
-- an absolute coord relative to (0,0,0). `Relative` means that it is an offset,
-- often, but not always, relative to a chunk's local (min x,min y = 0, min z)
type BlockCoord = BlockLocation 'Absolute
type BlockOffset = BlockLocation 'Relative

-- Have different Show instances to respect the difference as well.
instance Show BlockCoord where
  show (BlockLocation (x,y,z)) = "(Block)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

instance Show BlockOffset where
  show (BlockLocation (x,y,z)) = "(Offset)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

{- This is a bad instance that took almost an hour to debug. It still isn't fixed, just worked around (it's derived, and we don't rely on its ordering in Map.elems)
 - QuickCheck your instances, kids!
instance Ord BlockCoord where
  compare a@(BlockCoord (xa,ya,za)) b@(BlockCoord (xb,yb,zb)) = if a == b then EQ else if yb > ya then GT else if zb > za then GT else if xb > xa then GT else LT
-}

-- Only absolute coords can be serialized. This uses Minecraft's efficient
-- packing of coords into bits of an Int64 because the Y coord is very limited
-- in range compared to X and Z.
instance Serial BlockCoord where
  serialize (BlockLocation (x,y,z)) = serialize $
    -- 26 MSB
    (shiftL (u x .&. 0x3FFFFFF) 38) .|.
    -- 12 middle bits
    (shiftL (u y .&. 0xFFF) 26) .|.
    -- 26 LSB
    (u z .&. 0x3FFFFFF)
    where
      u = unsafeCoerce :: Int -> Int64
  -- Untested
  deserialize = (\w -> BlockLocation (u $ 0x3FFFFFF .&. (w `shiftR` 38),u $ 0xFFF .&. (w `shiftR` 26),u $ 0x3FFFFFF .&. w)) <$> deserialize @Int64
    where
      u = unsafeCoerce :: Int64 -> Int
-----------------------
-- Chunk Coordinates --
-----------------------

-- A Chunk Coord is a coordinate *of* a chunk slice, not *in* a chunk slice.
-- TODO: Investigate not using chunks at all, and only serializing to chunks on
-- demand. This can probably be very lazy and very efficient. Maybe even include
-- lazy chunk generation!
newtype ChunkCoord = ChunkCoord (Int,Int,Int) deriving (Eq,Ord)

-- This mirrors the format of `Show BlockLocation`
instance Show ChunkCoord where
  show (ChunkCoord (x,y,z)) = "(Chunk)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

----------
-- UUID --
----------

-- TODO: Investigate using a library to provide this type for seperation of concerns
-- There is no native Word128 type, so we role our own here.
newtype UUID = UUID (Word64,Word64)

instance Enum UUID where
  succ (UUID (a,b)) 
    | b /= maxBound = UUID (a, succ b)
    | otherwise = if a == maxBound 
      then error "Enum.succ{UUID}: tried to take `succ' of maxBound" 
      else UUID (succ a, minBound)
  pred (UUID (a,b)) 
    | b /= minBound = UUID (a, pred b)
    | otherwise = if a == minBound 
      then error "Enum.pred{UUID}: tried to take `pred' of minBound" 
      else UUID (pred a, maxBound) 
  toEnum n = UUID (0,fromIntegral n)
  fromEnum (UUID (_,n)) = fromIntegral n

-- This show instance should match the standard Minecraft display format.
instance Show UUID where
  show (UUID (ua,ub)) = reformat $ showHex ua (showHex ub "")
    where
      -- Add a hyphen at an index
      ins i s = let (a,b) = splitAt i s in a ++ "-" ++ b
      -- Reformat the uuid to what the client expects
      reformat = ins 8 . ins 12 . ins 16 . ins 20

-- A UUID is pretending to be a Word128, so just smoosh all the bits together
instance Serial UUID where
  serialize (UUID (a,b)) = putWord64be a >> putWord64be b
  deserialize = (UUID .) . (,) <$> getWord64be <*> getWord64be

instance Data.Aeson.Types.FromJSON UUID where
  parseJSON (Aeson.String s) = pure $ (\i -> UUID (fromInteger $ i .&. 0xFFFFFFFFFFFFFFFF,fromInteger $ shiftR i 8)) . fst . head . readHex . T.unpack $ s
  parseJSON x = Data.Aeson.Types.typeMismatch "UUID" x



--------------------
-- Notchian Enums --
--------------------

data Gamemode = Survival | Creative deriving Show

instance Serial Gamemode where
  serialize = putWord8 . \case
    Survival -> 0x00 
    Creative -> 0x01 
  deserialize = getWord8 >>= pure . \case
    0x00 -> Survival
    0x01 -> Creative
    x -> error $ "Deserialization error: deserialize @Gamemode called with " ++ show x

data Difficulty = Peaceful | Easy | Normal | Hard deriving Show

instance Serial Difficulty where
  serialize = putWord8 . \case
    Peaceful -> 0x00
    Easy -> 0x01
    Normal -> 0x02
    Hard -> 0x03
  deserialize = getWord8 >>= pure . \case
    0x00 -> Peaceful 
    0x01 -> Easy 
    0x02 -> Normal 
    0x03 -> Hard 
    x -> error $ "Deserialization error: deserialize @Difficulty called with " ++ show x

data Dimension = Nether | Overworld | TheEnd deriving Show

instance Serial Dimension where
  serialize = serialize . \case
    Nether -> (-1 :: Int32)
    Overworld -> (0 :: Int32)
    TheEnd -> (1 :: Int32)
  deserialize = getWord8 >>= pure . \case
    (-1) -> Nether
    0 -> Overworld
    1 -> TheEnd
    x -> error $ "Deserialization error: deserialize @Dimension called with " ++ show x

data Hand = MainHand | OffHand deriving (Show,Eq)

instance Serial Hand where
  serialize MainHand = serialize @VarInt 0
  serialize OffHand = serialize @VarInt 1
  deserialize = flip fmap (deserialize @VarInt) $ \case
    0 -> MainHand
    1 -> OffHand
    x -> error $ "deserialize @Hand: Got (VarInt) " <> show x

data BlockFace = Bottom | Top | SideFace CardinalDirection deriving Show

instance Serial BlockFace where
  serialize = serialize @VarInt . fromIntegral . fromEnum
  deserialize = toEnum . fromIntegral <$> deserialize @VarInt

instance Enum BlockFace where
  toEnum = \case
    0 -> Bottom
    1 -> Top
    n -> SideFace (toEnum n)
  fromEnum = \case
    Bottom -> 0
    Top -> 1
    SideFace f -> fromEnum f

data CardinalDirection = North | South | West | East deriving Show

instance Enum CardinalDirection where
  toEnum = \case
    2 -> North
    3 -> South
    4 -> West
    5 -> East
    _ -> error "toEnum CardinalDirection"
  fromEnum = \case
    North -> 2
    South -> 3
    West -> 4
    East -> 5

data MoveMode = Sprinting | Sneaking | Walking | Gliding | Flying


