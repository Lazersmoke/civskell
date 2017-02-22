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

import Control.Eff
import Crypto.Cipher.AES (AES128)
import Data.Bits
import Data.Int (Int16,Int32,Int64)
import Data.List (unfoldr,intercalate)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
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
import qualified Data.Set as Set

instance Serialize NBT where
  serialize = Ser.encode

protocolVersion :: Integer
protocolVersion = 316

-- Type Synonyms
type Short = Int16
type PlayerId = Int
-- Cipher, Enc, Dec
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- `HasIO` effect
type HasIO = Member IO
-- You make an effect that `HasIO` by `send`ing an IO action (`send (putStrLn "meow")`)

-- Simple way to inject a text message into a json chat string
jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"

-- Datatypes
-- Used in PlayerInfo to track the breaking stage of a block the player is mining
newtype BlockBreak = BlockBreak Word8

-- A block coordinate, not an entity position. Ord is derivied for use in maps.
newtype BlockCoord = BlockCoord (Int,Int,Int) deriving (Eq,Ord)

instance Show BlockCoord where
  show (BlockCoord (x,y,z)) = "(Block)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

{- This is a bad instance that took almost an hour to debug. It still isn't fixed, just worked around (it's derived, and we don't rely on its ordering in Map.elems)
instance Ord BlockCoord where
  compare a@(BlockCoord (xa,ya,za)) b@(BlockCoord (xb,yb,zb)) = if a == b then EQ else if yb > ya then GT else if zb > za then GT else if xb > xa then GT else LT
-}

newtype ChunkCoord = ChunkCoord (Int,Int,Int) deriving (Eq,Ord)

instance Show ChunkCoord where
  show (ChunkCoord (x,y,z)) = "(Chunk)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

-- VarInt's max size is an Int32, so we use that to store it. Note that they are also serialized different than a normal MCInt
newtype VarInt = VarInt {unVarInt :: Int32} deriving (Num,Bits,Eq,Enum,Integral,Real,Ord)

instance Show VarInt where
  show = show . unVarInt

-- Block id, count, damage
data Slot = EmptySlot | Slot Short Word8 Short (Maybe NbtContents)

instance Show Slot where
  show EmptySlot = "{}"
  show (Slot bid count dmg mNbt) = "{" ++ show count ++ " of [" ++ show bid ++ ":" ++ show dmg ++ "]" ++ n mNbt ++ "}"
    where
      -- Only display the NBT if it is actually there
      n Nothing = ""
      n (Just nbt) = " with tags: " ++ show nbt

-- Maps coords to non-air blocks. BlockState 0 0 doesn't mean anything. Air just doesn't have anything in the map.
data ChunkSection = ChunkSection (Map BlockCoord BlockState)

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
      merged :: Map BlockCoord BlockState
      merged = if Map.size (Map.union bs airChunk) == 4096 then Map.union bs airChunk else error "Bad Chunksection"

      -- `longChunks` is because Mojang likes to twist their data into weird shapes
      sArray :: BS.ByteString
      sArray = LBS.toStrict . longChunks . BB.toLazyByteString $ sBlockStates merged

      -- This should be the final result, but mojang is weird about chunk sections :S
      sBlockStates :: Map BlockCoord BlockState -> BB.BitBuilder
      sBlockStates m = foldl (\bb bc -> aBlock (Map.findWithDefault (BlockState 0 0) bc m) `BB.append` bb) BB.empty allCoords

      -- Annotate the data with its length in Minecraft Longs (should always be a whole number assuming 16^3 blocks/chunk)
      chunkData :: BS.ByteString
      chunkData = serialize (fromIntegral (BS.length sArray) `div` 8 :: VarInt) <> sArray

      -- Right now we `const 15` the blocks, so all the blocks are fullbright
      -- TODO: Add lighting information somewhere or derive it here
      createLights = Map.foldl (\bb _ -> BB.fromBits 4 (15 :: Word8) `BB.append` bb) BB.empty

      -- Mojang felt like packing chunks into arrays of longs lol
      longChunks :: LBS.ByteString -> LBS.ByteString
      longChunks unchunked = LBS.concat . reverse $ bsChunksOf 8 unchunked

      -- Helper function for chunking up the BS
      bsChunksOf :: Int64 -> LBS.ByteString -> [LBS.ByteString]
      bsChunksOf x = unfoldr (\a -> if not (LBS.null a) then Just (LBS.splitAt x a) else Nothing)

      -- Serialize a single block: 9 bits bid, 4 bits dmg
      aBlock :: BlockState -> BB.BitBuilder
      aBlock (BlockState bid dmg) = BB.fromBits 9 bid `BB.append` BB.fromBits 4 dmg

-- Enumerations for the protocol
data Side = Server | Client

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

-- Used in Server.PlayerDigging
data PlayerDigAction = StartDig BlockCoord BlockFace | StopDig BlockCoord BlockFace | EndDig BlockCoord BlockFace | DropItem Bool | ShootArrowOrFinishEating | SwapHands

-- Used in Server.EntityAction
data PlayerEntityAction = Sneak Bool | Sprint Bool | HorseJump Bool VarInt | LeaveBed | ElytraFly | HorseInventory

data BlockFace = Bottom | Top | North | South | West | East deriving Show

-- Used in Server.ClickWindow
data InventoryClickMode = NormalClick Bool | ShiftClick Bool | NumberKey Word8 | MiddleClick | ItemDropOut Bool | PaintingMode Word8 | DoubleClick deriving Show

-- Used in PlayerInfo
data MoveMode = Sprinting | Sneaking | Walking

-- Used in Client.ChangeGameState
data GameStateChange = InvalidBed | Raining Bool | ChangeGamemode Gamemode | ExitTheEnd Bool | DemoMessage | ArrowHitOtherPlayer | FadeValue Float | FadeTime Float | ElderGuardian

blockToChunk :: BlockCoord -> ChunkCoord
blockToChunk (BlockCoord (x,y,z)) = ChunkCoord (x `div` 16,y `div` 16,z `div` 16)

blockToRelative :: BlockCoord -> BlockCoord
blockToRelative (BlockCoord (x,y,z)) = BlockCoord (f x,f y,f z)
  where
    -- We don't use negative relative coords in negative chunks
    f n = if mod n 16 < 0 then 16 + mod n 16 else mod n 16

-- This is just for name sharing. We never have a constraint (PacketId p) => p
class PacketId p where
  packetName :: p -> String
  packetId :: p -> VarInt
  packetSide :: p -> Side
  packetState :: p -> ServerState

blockInChunk :: BlockCoord -> ChunkSection -> BlockState
blockInChunk b (ChunkSection m) = fromMaybe (BlockState 0 0) (Map.lookup b m)

-- Block id, damage
data BlockState = BlockState Short Word8 deriving Eq

instance Show BlockState where
  show (BlockState bid dmg) = "Block [" ++ show bid ++ ":" ++ show dmg ++ "]"

-- Things that can be serialized into a BS for the network
-- Show req lets us print them if need be
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

allCoords :: [BlockCoord]
allCoords = [BlockCoord (x,y,z) | y <- [0..15], z <- [0..15], x <- [0..15]]

airChunk :: Map BlockCoord BlockState
airChunk = Map.fromList [(BlockCoord (x,y,z),BlockState 0 0) | x <- [0..15], z <- [0..15], y <- [0..15]]

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
  ,clientBrand :: String
  ,clientUsername :: String
  ,clientUUID :: String
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
  ,clientBrand = ""
  ,clientUsername = ""
  ,clientUUID = ""
  ,holdingSlot = 0
  ,playerPosition = (0,0,0)
  ,viewAngle = (0,0)
  ,gameMode = Survival
  ,playerInventory = Map.empty
  ,diggingBlocks = Map.empty
  ,moveMode = Walking
  }
