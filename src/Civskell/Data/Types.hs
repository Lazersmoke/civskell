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

liftIO :: HasIO r => IO a -> Eff r a
liftIO = send

type HasIO = Member IO
-- Cipher, Enc, Dec
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- Enums for parsing, etc
data Side = Server | Client deriving Show

data ServerState = Handshaking | Playing | LoggingIn | Status deriving Show

data Gamemode = Survival | Creative deriving Show

instance Serialize Gamemode where
  serialize Survival = BS.singleton 0x00
  serialize Creative = BS.singleton 0x01

data Difficulty = Peaceful | Easy | Normal | Hard deriving (Eq,Show)

instance Serialize Difficulty where
  serialize Peaceful = BS.singleton 0x00
  serialize Easy = BS.singleton 0x01
  serialize Normal = BS.singleton 0x02
  serialize Hard = BS.singleton 0x03

data Hand = MainHand | OffHand deriving (Show,Eq,Enum)

data AnimationAction = SwingHand Hand | Critical Bool | TakeDamage | LeaveBedAnimation deriving Show

data PlayerDigAction = StartDig BlockCoord BlockFace | StopDig BlockCoord BlockFace | EndDig BlockCoord BlockFace | DropItem Bool | ShootArrowOrFinishEating | SwapHands deriving (Show,Eq)

data PlayerEntityAction = Sneak Bool | Sprint Bool | HorseJump Bool VarInt | LeaveBed | ElytraFly | HorseInventory

data BlockFace = Bottom | Top | North | South | West | East deriving (Show,Eq)

data InventoryClickMode = NormalClick Bool | ShiftClick Bool | NumberKey Word8 | MiddleClick | ItemDropOut Bool | PaintingMode Word8 | DoubleClick deriving (Show,Eq)

data MoveMode = Sprinting | Sneaking | Walking deriving (Show,Eq)

data GameStateChange = InvalidBed | Raining Bool | ChangeGamemode Gamemode | ExitTheEnd Bool | DemoMessage | ArrowHitOtherPlayer | FadeValue Float | FadeTime Float | ElderGuardian

newtype BlockBreak = BlockBreak Word8 deriving (Show,Eq)

newtype BlockCoord = BlockCoord (Int,Int,Int) deriving (Eq)

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
class {-Show s =>-} Serialize s where
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
  serialize (BlockState bid dmg) = serialize $ shiftL (u bid) 4 .|. (v dmg .&. 0x0f)
    where
      u = unsafeCoerce :: Short -> VarInt
      v = unsafeCoerce :: Word8 -> VarInt

instance Serialize ChunkSection where
  serialize (ChunkSection bs) = serialize bitsPerBlock <> sPalette <> sData <> lights <> lights
    where
      lights = LBS.toStrict $ BB.toLazyByteString $ writeDat $ map (const 0) blockStateList
      bitsPerBlock = 13 :: VarInt
      --palette = nub dataArray
      sPalette = BS.singleton 0x00 -- withLength $ sBlockStates palette
      blockStateList = Map.elems (Map.union bs airChunk)
      dataArray = blockStateList -- map ((\(Just a) -> a) . flip elemIndex palette . fst) bs
      sArray = LBS.toStrict . longChunks . BB.toLazyByteString $ sBlockStates dataArray
      sData = serialize (fromIntegral (BS.length sArray) `div` 8 :: VarInt) <> sArray
      writeDat :: [Word8] -> BB.BitBuilder
      writeDat (x:y:xs) = writeDat xs `BB.append` BB.fromBits 4 y `BB.append` BB.fromBits 4 x
      writeDat [] = BB.empty
      writeDat (_:[]) = error $ "Bad chunksection block list size: " ++ show (Map.size $ Map.union bs airChunk) ++ " | " ++ show (Map.union bs airChunk)

airChunk :: Map BlockCoord BlockState
airChunk = Map.fromList [(BlockCoord (x,y,z),BlockState 0 0) | x <- [0..15], y <- [0..15], z <- [0..15]]

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

type HasNetworking = Member Networking

data Networking a where
  SetCompressionLevel :: Maybe VarInt -> Networking ()
  AddCompression :: BS.ByteString -> Networking BS.ByteString
  RemoveCompression :: BS.ByteString -> Networking BS.ByteString
  SetupEncryption :: EncryptionCouplet -> Networking ()
  GetFromNetwork :: Int -> Networking BS.ByteString
  PutIntoNetwork :: BS.ByteString -> Networking ()

rGet :: HasNetworking n => Int -> Eff n BS.ByteString
rGet = send . GetFromNetwork

rPut :: HasNetworking n => BS.ByteString -> Eff n ()
rPut = send . PutIntoNetwork

setupEncryption :: HasNetworking n => EncryptionCouplet -> Eff n ()
setupEncryption = send . SetupEncryption

setCompression :: HasNetworking n => Maybe VarInt -> Eff n ()
setCompression = send . SetCompressionLevel

addCompression :: HasNetworking n => BS.ByteString -> Eff n BS.ByteString
addCompression = send . AddCompression

removeCompression :: HasNetworking n => BS.ByteString -> Eff n BS.ByteString
removeCompression = send . RemoveCompression

formatPacket :: String -> [(String,String)] -> String
formatPacket n [] = "{" ++ n ++ "}"
formatPacket n xs = flip (++) "]" . (++) ("{" ++ n ++ "} [") . intercalate " | " . map (\(name,val) -> if val == "" then name else name ++ ": " ++ val) $ xs

-- Annotate a BS with its length as a VarInt
withLength :: BS.ByteString -> BS.ByteString
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) <> bs

withListLength :: Serialize s => [s] -> BS.ByteString
withListLength ls = serialize ((fromIntegral $ length ls) :: VarInt) <> BS.concat (map serialize ls)

data PlayerInfo = PlayerInfo
  {teleportConfirmationQue :: Set.Set VarInt
  ,nextTid :: VarInt
  ,keepAliveQue :: Set.Set VarInt
  ,nextKid :: VarInt
  ,clientBrand :: Maybe String
  ,holdingSlot :: Short
  ,playerPosition :: (Double,Double,Double)
  ,viewAngle :: (Float,Float)
  ,gameMode :: Gamemode
  ,playerInventory :: Map Short Slot
  ,diggingBlocks :: Map BlockCoord BlockBreak
  ,moveMode :: MoveMode
  }

defaultPlayerInfo :: PlayerInfo
defaultPlayerInfo = PlayerInfo
  {teleportConfirmationQue = Set.empty
  ,nextTid = 0
  ,keepAliveQue = Set.empty
  ,nextKid = 0
  ,clientBrand = Nothing
  ,holdingSlot = 0
  ,playerPosition = (0,0,0)
  ,viewAngle = (0,0)
  ,gameMode = Survival
  ,playerInventory = Map.empty
  ,diggingBlocks = Map.empty
  ,moveMode = Walking
  }

