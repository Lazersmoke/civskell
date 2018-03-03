{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Civskell.Data.Chunk where
import Control.Lens
import Data.List
import Civskell.Data.Protocol
import Civskell.Data.Common
import Civskell.Data.Block
import Data.SuchThat
import Data.Bytes.Serial
import qualified Data.Binary.BitBuilder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.Bytes.Put
import qualified Data.Array.IArray as Arr
import GHC.Generics hiding (to)
import Data.Word
-- TODO: Investigate storing just the block data (no descriptors) in the structure.
-- Maps coords to blocks in a single chunk section.
newtype ChunkSection b = ChunkSection {unChunkSection :: (Arr.Array (Nibble,Nibble,Nibble) b)} deriving (Generic)

instance Rewrapped (ChunkSection b) t
instance Wrapped (ChunkSection b) where
  type Unwrapped (ChunkSection b) = Arr.Array (Nibble,Nibble,Nibble) b

-- All the relative coords within a chunk
allCoords :: [BlockOffset]
allCoords = [BlockLocation (x,y,z) | y <- [0..15], z <- [0..15], x <- [0..15]]

emptyChunk :: ChunkSection (ForAny Block)
emptyChunk = ChunkSection $ Arr.listArray ((0,0,0),(15,15,15)) (repeat . ambiguate $ Block air Air)

chunkToWireBlock :: ChunkSection (ForAny Block) -> ChunkSection WireBlock
chunkToWireBlock = _Wrapped . mapped %~ (ambiguously toWireBlock)


-- Derived instance is really long (it shows all the associations of coords and
-- blocks in a giant list), so we replace it with a short placeholder
instance Show (ChunkSection b) where
  show _ = "<Chunk Section>"

-- TODO: Add paletteing. Right now, we send a full 13 bits for *every block*
-- Lights twice for (((reasons)))
instance Serial (ChunkSection WireBlock) where
  serialize chunkSection = serialize bitsPerBlock >> putWord8 0x00 >> chunkData >> lights >> lights
    where
      lights = putLazyByteString . BB.toLazyByteString . createLights . unChunkSection $ chunkSection

      -- First 9 bits are the block id, last 4 are the damage
      bitsPerBlock = 13 :: VarInt

      -- Send 0x00 as the length of the palette since we aren't using it
      --palette = BS.singleton 0x00

      -- `longChunks` is because Mojang likes to twist their data into weird shapes
      -- Might need to remove it idk
      sArray :: LBS.ByteString
      sArray = longChunks . BB.toLazyByteString . sBlockStates . unChunkSection $ chunkSection

      -- This should be the final result, but mojang is weird about chunk sections :S
      sBlockStates :: Arr.Array (Nibble,Nibble,Nibble) WireBlock -> BB.BitBuilder
      sBlockStates = foldr (\newBlock bb -> aBlock newBlock `BB.append` bb) BB.empty

      -- Annotate the data with its length in Minecraft Longs (should always be a whole number assuming 16^3 blocks/chunk)
      chunkData :: MonadPut m => m ()
      chunkData = serialize (fromIntegral (LBS.length sArray) `div` 8 :: VarInt) >> putLazyByteString sArray

      -- Right now we `const 15` the blocks, so all the blocks are fullbright
      -- TODO: Add lighting information somewhere or derive it here
      createLights = foldl' (\bb _ -> BB.fromBits 4 (15 :: Word8) `BB.append` bb) BB.empty

      -- Mojang felt like packing chunks into arrays of longs lol
      longChunks :: LBS.ByteString -> LBS.ByteString
      longChunks unchunked = LBS.concat . reverse $ bsChunksOf 8 unchunked

      -- Helper function for chunking up the BS
      bsChunksOf :: Int64 -> LBS.ByteString -> [LBS.ByteString]
      bsChunksOf x = unfoldr (\a -> if not (LBS.null a) then Just (LBS.splitAt x a) else Nothing)

      -- Serial a single block: 9 bits bid, 4 bits dmg
      aBlock :: WireBlock -> BB.BitBuilder
      aBlock (WireBlock bId meta) = BB.fromBits 9 bId `BB.append` BB.fromBits 4 meta
  deserialize = error "Unimplemented: Deserialization of Chunk Sections"

data Air = Air deriving (Show,Eq)

air :: BlockDescriptor Air
air = BlockDescriptor
  {blockId = 0
  ,blockIdentifier = "minecraft:air"
  ,blockName = const "Air"
  ,blockMeta = const 0
  }


