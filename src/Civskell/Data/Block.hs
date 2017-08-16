{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Civskell.Data.Block where

import Control.Monad.Freer
import Data.Attoparsec.ByteString
import Control.Monad
import qualified Data.Binary.BitBuilder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Bytes.Serial

import Civskell.Tech.Parse
--import Civskell.Data.Player
--import Civskell.Data.World
import Civskell.Data.Logging
import Civskell.Data.Types
import Civskell.Data.Protocol

-- TODO: Reconsider sparsity obligations. This is pretty dumb in terms of invariants we must enforce.
-- TODO: Reconsider using Map because that is honestly a retarded data structure to use here
-- TODO: Why isn't this a newtype. This is dumb :(
-- Maps coords to non-air blocks. BlockState 0 0 doesn't mean anything. Air just doesn't have anything in the map.
data ChunkSection = ChunkSection (Map BlockOffset (Some Block))

-- All the relative coords within a chunk
allCoords :: [BlockOffset]
allCoords = [BlockLocation (x,y,z) | y <- [0..15], z <- [0..15], x <- [0..15]]

-- A chunk full of air (used in chunk serialization to fill in gaps in the Map)
airChunk :: Map BlockOffset (Some Block)
airChunk = Map.fromList [(BlockLocation (x,y,z),some Air) | x <- [0..15], z <- [0..15], y <- [0..15]]

-- Get the block at a location in a chunk
blockInChunk :: BlockOffset -> ChunkSection -> Some Block
blockInChunk b (ChunkSection m) = fromMaybe (some Air) (Map.lookup b m)

-- Derived instance is really long (it shows all the associations of coords and
-- blocks in a giant list), so we replace it with a short placeholder
instance Show ChunkSection where
  show _ = "<Chunk Section>"

-- TODO: Add paletteing. Right now, we send a full 13 bits for *every block*
-- Lights twice for (((reasons)))
instance Serial ChunkSection where
  serialize (ChunkSection bs) = serialize bitsPerBlock >> putWord8 0x00 >> chunkData >> lights >> lights
    where
      lights = putLazyByteString . BB.toLazyByteString . createLights $ merged

      -- First 9 bits are the block id, last 4 are the damage
      bitsPerBlock = 13 :: VarInt

      -- Send 0x00 as the length of the palette since we aren't using it
      --palette = BS.singleton 0x00

      -- Notify and crash *right away* if this gets fucked up. This only happens if we have a bad Ord instance.
      -- Note that Map.size is O(1) so we aren't wasting too much time here
      -- Merge the chunk's blocks with air because air blocks don't exist in the chunk normally
      merged :: Map BlockOffset (Some Block)
      merged = if Map.size (Map.union bs airChunk) == 4096 then Map.union bs airChunk else error "Bad Chunksection"

      -- `longChunks` is because Mojang likes to twist their data into weird shapes
      -- Might need to remove it idk
      sArray :: LBS.ByteString
      sArray = longChunks . BB.toLazyByteString $ sBlockStates merged

      -- This should be the final result, but mojang is weird about chunk sections :S
      sBlockStates :: Map BlockOffset (Some Block) -> BB.BitBuilder
      sBlockStates m = foldr (\bc bb -> ambiguously (aBlock . runIdentity) (Map.findWithDefault (some Air) bc m) `BB.append` bb) BB.empty allCoords

      -- Annotate the data with its length in Minecraft Longs (should always be a whole number assuming 16^3 blocks/chunk)
      chunkData :: MonadPut m => m ()
      chunkData = serialize (fromIntegral (LBS.length sArray) `div` 8 :: VarInt) >> putLazyByteString sArray

      -- Right now we `const 15` the blocks, so all the blocks are fullbright
      -- TODO: Add lighting information somewhere or derive it here
      createLights = Map.foldl' (\bb _ -> BB.fromBits 4 (15 :: Word8) `BB.append` bb) BB.empty

      -- Mojang felt like packing chunks into arrays of longs lol
      longChunks :: LBS.ByteString -> LBS.ByteString
      longChunks unchunked = LBS.concat . reverse $ bsChunksOf 8 unchunked

      -- Helper function for chunking up the BS
      bsChunksOf :: Int64 -> LBS.ByteString -> [LBS.ByteString]
      bsChunksOf x = unfoldr (\a -> if not (LBS.null a) then Just (LBS.splitAt x a) else Nothing)

      -- Serial a single block: 9 bits bid, 4 bits dmg
      aBlock :: Block b => b -> BB.BitBuilder
      aBlock (b :: bt) = BB.fromBits 9 (blockId @bt) `BB.append` BB.fromBits 4 (blockMeta b)
  deserialize = error "Unimplemented: Deserialization of Chunk Sections"


class Block b where
  -- 1
  blockId :: Short
  -- "minecraft:stone"
  blockIdentifier :: String
  -- Stone -> 0; Granite -> 1
  blockMeta :: b -> Nibble
  blockMeta _ = 0
  -- By default, blocks without meta get 0
  --blockMeta _ = 0
  -- Name in notchian english "Stone" or "Granite"
  blockName :: b -> String
  -- Some blocks do something when clicked
  onClick :: forall r. (HasWorld r,HasPlayer r,SendsPackets r,Logs r) => Maybe (b -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Eff r ())
  onClick = Nothing
  droppedItem :: Maybe (b -> Some Item)

serializeBlock :: (MonadPut m,Block b) => b -> m ()
serializeBlock (b :: bt) = serialize $ shiftL (u (blockId @bt)) 4 .|. (v (blockMeta b) .&. 0x0f)
  where
    u = unsafeCoerce :: Short -> VarInt
    v = unsafeCoerce :: Nibble -> VarInt

showBlock :: Block b => b -> String
showBlock (b :: bt) = "Block [" ++ show (blockId @bt) ++ ":" ++ show (blockMeta b) ++ "]"


-- Parse an item based on its blockId, assuming no metadata and no nbt
standardParser :: forall i. Item i => i -> Parser Slot
standardParser r = do
  bid <- parseShort
  guard $ bid == itemId @i
  cnt <- anyWord8
  dmg <- parseShort
  guard $ dmg == 0
  nbtFlair <- anyWord8
  guard $ nbtFlair == 0
  return . Slot . Just $ SlotData (some r) cnt


placeBlock :: (HasWorld r,SendsPackets r,HasPlayer r,Logs r,Block b) => b -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Eff r ()
placeBlock b bc bf hand _ = do
  -- Find out what item they are trying to place
  heldSlot <- if hand == MainHand then (+36) . holdingSlot <$> getPlayer else pure 45
  getInventorySlot heldSlot >>= \case
    Slot Nothing -> loge "Fight me; this is not possible! (literally Nothing has a `placeBlock` callback)"
    Slot (Just (SlotData i icount)) -> do
      -- Remove item from inventory
      let newSlot = if icount == 1 then Slot Nothing else Slot $ Just (SlotData i (icount - 1))
      setInventorySlot heldSlot newSlot
      setBlock b (blockOnSide bc bf)


