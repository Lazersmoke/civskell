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
module Civskell.Data.Common where

import Data.Semigroup
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import qualified Data.ByteString as BS
import Data.Int (Int32,Int64)
import Data.Bits
import Data.Word
import qualified Data.Vector as Vector
import GHC.Generics
import Data.Text (Text)
import Data.SuchThat
import qualified Data.Text as T
import Unsafe.Coerce (unsafeCoerce)
import Hexdump (prettyHex)

import Civskell.Data.Protocol

-- * Packets

-- | A @'PacketDescriptor' h p@ is a description of a packet of type @p@, parameterized by a
-- handler @h@ which is typically either a way to serialize the packet (clientbound) or handle the packet (serverbound)
data PacketDescriptor h p = PacketDescriptor 
  {packetPretty :: p -> [(Text,Text)]
  ,packetName :: Text
  ,packetState :: ServerState
  ,packetHandler :: h p
  }

-- | An @'OutboundPacketDescriptor' p@ is a @'PacketDescriptor'@ for clientbound packets
type OutboundPacketDescriptor = PacketDescriptor PacketSerializer

-- | A @'DescribedPacket' h p@ is a packet @p@ together with its descriptor of type @'PacketDescriptor' h p@
data DescribedPacket h p = DescribedPacket (PacketDescriptor h p) p

-- | The mode under which an incoming packet should be handled.
-- Packets that are capable of updating the ServerState *must* be marked as @'SerialThreading'@.
-- Marking a packet as @'ParThreading'@ means that it will be handled in its own (green) thread.
data ThreadingMode = SerialThreading | ParThreading

-- | A @'PacketSerializer' p@ is a description of how to serialize a packet of type @p@, including its @'packetId'@.
data PacketSerializer p = PacketSerializer 
  {packetId :: VarInt
  ,serializePacket :: forall m. MonadPut m => p -> m ()
  }

-- | The set of packets supported by this particular configuration.
-- The order determines the packet ids, and is therefore significant.
type SupportedPackets h = Vector.Vector (SuchThat '[Serial] (PacketDescriptor h))

-- | Move a single element in a vector from one place to another, reshuffling as needed.
-- This is useful because Mojang sometimes reorganizes their packets in a way similar or identical to this.
moveVec :: Int -> Int -> Vector.Vector a -> Vector.Vector a
moveVec old new v = Vector.concat [before,between,Vector.singleton oldelem,after]
  where
    before = Vector.take old v
    oldelem = v Vector.! old
    between = Vector.slice (old + 1) (new - old - 1) v
    after = Vector.slice (new + 1) (Vector.length v - new - 1) v 

-- | We can't make this an actual `Show` instance without icky extentions. Also,
-- the instance context is not checked during instance selection, so we would
-- have to make a full on `instance Show a where`, which is obviously bad.
showPacket :: PacketDescriptor h p -> p -> Text
showPacket pktDesc p = formatPacket (packetName pktDesc) (packetPretty pktDesc p)

-- | Helper function to format Packets during logging.
-- Takes a packet name and a list of properties (name value pairs).
-- Empty values only show the name.
formatPacket :: Text -> [(Text,Text)] -> Text
formatPacket n [] = "{" <> n <> "}"
formatPacket n xs = flip (<>) "]" . (<>) ("{" <> n <> "} [") . T.intercalate " | " . map (\(name,val) -> if val == "" then name else name <> ": " <> val) $ xs

-- | Packets are ambiguous in the Notchian spec, so we need to carry around a
-- @'ServerState'@ that tells us how to parse the packets.
data ServerState = Handshaking | Playing | LoggingIn | Status deriving (Generic,Eq)

-- * Block Coordinates

-- | A block coordinate, not an entity position. @'Ord'@ is derivied for use in maps,
-- but does not represent any specific concrete ordering, and should not be
-- relied on for serialization to a given format.
data BlockLocation (r :: Relativity)
  = BlockLocation (Int,Int,Int) -- (x,y,z)
  deriving (Eq,Ord)

-- | Kind level annotation for block positions. See @'BlockCoord'@ and @'BlockOffset'@.
data Relativity = Relative | Absolute

-- | @'Absolute'@ or @'BlockCoord'@ means that it is an absolute coord relative to (0,0,0).
type BlockCoord = BlockLocation 'Absolute
-- |  @'Relative'@ or @'BlockOffset'@ means that it is an offset, often, but not always, relative to a chunk's local (min x,min y, min z).
type BlockOffset = BlockLocation 'Relative

-- | (Block)<x,y,z>
instance Show BlockCoord where
  show (BlockLocation (x,y,z)) = "(Block)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

-- | (Offset)<x,y,z>
instance Show BlockOffset where
  show (BlockLocation (x,y,z)) = "(Offset)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

{- Preserved for posterity
 - This is a bad instance that took almost an hour to debug. It still isn't fixed, just worked around (it's derived, and we don't rely on its ordering in Map.elems)
 - QuickCheck your instances, kids!
instance Ord BlockCoord where
  compare a@(BlockCoord (xa,ya,za)) b@(BlockCoord (xb,yb,zb)) = if a == b then EQ else if yb > ya then GT else if zb > za then GT else if xb > xa then GT else LT
-}

-- | Only absolute coords can be serialized. This uses Minecraft's efficient
-- packing of coords into bits of an @'Int64'@ because the Y coord is very limited
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

-- The state of a block in the world (Block id, damage)
data BlockState = BlockState Short Word8 deriving Eq

-- * Chunk Coordinates

-- A Chunk Coord is a coordinate *of* a chunk slice, not *in* a chunk slice.
-- TODO: Investigate not using chunks at all, and only serializing to chunks on
-- demand. This can probably be very lazy and very efficient. Maybe even include
-- lazy chunk generation!
newtype ChunkCoord = ChunkCoord (Int,Int,Int) deriving (Eq,Ord)

-- This mirrors the format of `Show BlockLocation`
instance Show ChunkCoord where
  show (ChunkCoord (x,y,z)) = "(Chunk)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

-- * Notchian Enums

-- | Gamemode as in Vanilla Minecraft. Incomplete right now.
data Gamemode = Survival | Creative deriving Show

instance Serial Gamemode where
  serialize = putWord8 . \case
    Survival -> 0x00 
    Creative -> 0x01 
  deserialize = getWord8 >>= pure . \case
    0x00 -> Survival
    0x01 -> Creative
    x -> error $ "deserialize @Gamemode: Unknown gamemode " ++ show x

-- | Difficulty, as in Vanilla Minecraft. Hardcore is independent of this.
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
    x -> error $ "deserialize @Difficulty: Unknown difficulty level: " ++ show x

-- | Dimension, as in Vanilla Minecraft, except it is enumerated instead of a number.
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
    x -> error $ "deserialize @Dimension: Unknown dimension " ++ show x

-- | Hands for e.g. dual wielding.
data Hand = MainHand | OffHand deriving (Show,Eq)

instance Serial Hand where
  serialize MainHand = serialize @VarInt 0
  serialize OffHand = serialize @VarInt 1
  deserialize = flip fmap (deserialize @VarInt) $ \case
    0 -> MainHand
    1 -> OffHand
    x -> error $ "deserialize @Hand: Got (VarInt) " <> show x

-- | One of the six faces of a block, see also @'CardinalDirection'@.
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

-- | Cardinal directions, in standard Minecraft ordering.
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

-- | The movement mode of a player.
data MoveMode = Sprinting | Sneaking | Walking | Gliding | Flying

-- | Literally @'T.pack' . 'show'@.
showText :: Show a => a -> T.Text
showText = T.pack . show

-- Indent the hexdump; helper function
indentedHex :: BS.ByteString -> String
indentedHex = init . unlines . map ("  "++) . lines . prettyHex
