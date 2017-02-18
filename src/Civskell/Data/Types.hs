{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Civskell.Data.Types where

import Data.Int
import Data.Word
import Data.Maybe
import Data.Bits
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Binary.BitBuilder as BB
import Data.List
import Data.Semigroup
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8
import Unsafe.Coerce
import Control.Eff
import Crypto.Cipher.AES
import Data.NBT
import qualified Data.Serialize as Ser

instance Serialize NBT where
  serialize = Ser.encode

-- VarInt's are parsed into Int32's
newtype VarInt = VarInt {unVarInt :: Int32} deriving (Num,Bits,Eq,Enum,Integral,Real,Ord)
instance Show VarInt where
  show = show . unVarInt

type Short = Int16

type PlayerId = Int

type HasIO = Member IO
type HasNetworking = Member Networking
type HasLogging = Member Logging
type HasPlayer = Member Player
type HasWorld = Member World
-- Cipher, Enc, Dec
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- Enums for parsing, etc
data Side = Server | Client

data ServerState = Handshaking | Playing | LoggingIn | Status

data Gamemode = Creative

newtype BlockCoord = BlockCoord (Int,Int,Int) deriving Eq
instance Show BlockCoord where
  show (BlockCoord (x,y,z)) = "(Block)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"
instance Ord BlockCoord where
  compare a@(BlockCoord (xa,ya,za)) b@(BlockCoord (xb,yb,zb)) = if a == b then EQ else if yb > ya then GT else if zb > za then GT else if xb > xa then GT else LT
newtype ChunkCoord = ChunkCoord (Int,Int,Int) deriving (Eq,Ord)
instance Show ChunkCoord where
  show (ChunkCoord (x,y,z)) = "(Chunk)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

blockToChunk :: BlockCoord -> ChunkCoord
blockToChunk (BlockCoord (x,y,z)) = ChunkCoord (x `div` 16,y `div` 16,z `div` 16)

blockToRelative :: BlockCoord -> BlockCoord
blockToRelative (BlockCoord (x,y,z)) = BlockCoord (x `mod` 16,y `mod` 16,z `mod` 16)

-- Things that have a packet ID and a bound to side
class PacketId p where
  packetName :: p -> String
  packetId :: p -> VarInt
  packetSide :: p -> Side
  packetState :: p -> ServerState

-- Block id, count, damage
data Slot = EmptySlot | Slot Short Word8 Short (Maybe NbtContents) deriving Show
data ChunkSection = ChunkSection (Map BlockCoord BlockState) deriving Eq
instance Show ChunkSection where
  show _ = "<Chunk Section>"

blockInChunk :: BlockCoord -> ChunkSection -> BlockState
blockInChunk b (ChunkSection m) = fromMaybe (BlockState 0 0) (Map.lookup b m)

-- Block id, damage
data BlockState = BlockState Short Word8 deriving (Eq,Show)

-- Things that can be serialized into a BS for the network
-- Show req lets us print them if need be (might be removed later)
class Show s => Serialize s where
  serialize :: s -> BS.ByteString

-- Instances for Haskell types
-- Defined here so we don't orphan them
instance Serialize String where
  serialize str = serialize ((fromIntegral $ BS.length encoded) :: VarInt) `BS.append` encoded
    where
      encoded = Data.ByteString.UTF8.fromString str

instance Serialize Slot where
  serialize EmptySlot = serialize (-1 :: Short)
  serialize (Slot bid count dmg (Just nbt)) = serialize bid <> serialize count <> serialize dmg <> serialize (NBT "" nbt)
  serialize (Slot bid count dmg Nothing) = serialize bid <> serialize count <> serialize dmg <> BS.singleton 0x00

instance Serialize BlockState where
  serialize (BlockState bid dmg) = BS.pack [u (shiftR bid 4),u (shiftL bid 4) .|. dmg]
    where
      u = unsafeCoerce :: Short -> Word8

instance Serialize ChunkSection where
  serialize (ChunkSection bs) = serialize bitsPerBlock <> sPalette <> sData <> lights <> lights
    where
      lights = LBS.toStrict $ BB.toLazyByteString $ writeDat $ map (const 0) blockStateList
      bitsPerBlock = 13 :: VarInt
      --palette = nub dataArray
      sPalette = BS.singleton 0x00 -- withLength $ sBlockStates palette
      blockStateList = map snd (Map.toList bs)
      dataArray = blockStateList -- map ((\(Just a) -> a) . flip elemIndex palette . fst) bs
      sArray = LBS.toStrict . longChunks . BB.toLazyByteString $ sBlockStates dataArray
      sData = serialize (fromIntegral (BS.length sArray) `div` 8 :: VarInt) <> sArray
      writeDat :: [Word8] -> BB.BitBuilder
      writeDat (x:y:xs) = writeDat xs `BB.append` BB.fromBits 4 y `BB.append` BB.fromBits 4 x
      writeDat [] = BB.empty
      writeDat (_:[]) = error "Bad chunksection block list size"

-- Mojang felt like packing chunks into arrays of longs lol
longChunks :: LBS.ByteString -> LBS.ByteString
longChunks bs = LBS.reverse . LBS.concat . map LBS.reverse $ bsChunksOf 8 bs

bsChunksOf :: Int64 -> LBS.ByteString -> [LBS.ByteString]
bsChunksOf x = unfoldr (\a -> if not (LBS.null a) then Just (LBS.splitAt x a) else Nothing)

-- This should be the final result, but mojang is weird about chunk sections :S
sBlockStates :: [BlockState] -> BB.BitBuilder
sBlockStates ((BlockState bid dmg):bs) = sBlockStates bs `BB.append` BB.fromBits 9 bid `BB.append` BB.fromBits 4 dmg
sBlockStates [] = BB.empty

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

instance Serialize BlockCoord where
  serialize (BlockCoord (x,y,z)) = serialize $ 
    (shiftL (u x .&. 0x3FFFFFF) 38) .|. 
    (shiftL (u y .&. 0xFFF) 26) .|.
    (u z .&. 0x3FFFFFF)
    where
      u = unsafeCoerce :: Int -> Int64


