{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Civskell.Tech.Serialization where

import Data.Bits
import Data.Int (Int16,Int32,Int64)
import Data.List (unfoldr,foldl')
import Data.Map.Lazy (Map)
import Data.NBT
import Data.Semigroup ((<>))
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Binary.BitBuilder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8
import qualified Data.Map.Lazy as Map
import qualified Data.Serialize as Ser

import Civskell.Data.Types

instance Serialize NBT where
  serialize = Ser.encode

instance Serialize Slot where
  serialize EmptySlot = serialize (-1 :: Short)
  serialize (Slot bid count dmg (Just nbt)) = serialize bid <> serialize count <> serialize dmg <> serialize (NBT "" nbt)
  serialize (Slot bid count dmg Nothing) = serialize bid <> serialize count <> serialize dmg <> BS.singleton 0x00

instance Serialize Gamemode where
  serialize Survival = BS.singleton 0x00
  serialize Creative = BS.singleton 0x01
  -- serialize Adventure = BS.singleton 0x02
  -- serialize Spectator = BS.singleton 0x03

instance Serialize Difficulty where
  serialize Peaceful = BS.singleton 0x00
  serialize Easy = BS.singleton 0x01
  serialize Normal = BS.singleton 0x02
  serialize Hard = BS.singleton 0x03

instance Serialize AnimationAction where
  serialize (SwingHand MainHand) = BS.singleton 0x00
  serialize TakeDamage = BS.singleton 0x01
  serialize LeaveBedAnimation = BS.singleton 0x02
  serialize (SwingHand OffHand) = BS.singleton 0x03
  serialize (Critical False) = BS.singleton 0x04
  serialize (Critical True) = BS.singleton 0x05

instance Serialize BlockState where
  serialize (BlockState bid dmg) = serialize $ shiftL (u bid) 4 .|. (v dmg .&. 0x0f)
    where
      u = unsafeCoerce :: Short -> VarInt
      v = unsafeCoerce :: Word8 -> VarInt

instance Serialize (Block r) where
  serialize (Block (x,y,z)) = serialize $
    (shiftL (u x .&. 0x3FFFFFF) 38) .|.
    (shiftL (u y .&. 0xFFF) 26) .|.
    (u z .&. 0x3FFFFFF)
    where
      u = unsafeCoerce :: Int -> Int64

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

instance Serialize EntityId where serialize = serialize . unEID
instance Serialize WindowId where serialize = serialize . unWID

-- Instances for Haskell types
-- Defined here so we don't orphan them
instance Serialize [Char] where
  serialize str = serialize ((fromIntegral $ BS.length encoded) :: VarInt) `BS.append` encoded
    where
      encoded = Data.ByteString.UTF8.fromString str

instance Serialize Word8 where
  serialize = BS.singleton

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

instance Serialize Double where
  serialize = serialize . (unsafeCoerce :: Double -> Int64)


-- Annotate a BS with its length as a VarInt
withLength :: BS.ByteString -> BS.ByteString
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) <> bs

withListLength :: Serialize s => [s] -> BS.ByteString
withListLength ls = serialize ((fromIntegral $ length ls) :: VarInt) <> BS.concat (map serialize ls)

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

