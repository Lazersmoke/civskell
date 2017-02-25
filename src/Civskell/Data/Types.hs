{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Civskell.Data.Types where

import Control.Eff
import Crypto.Cipher.AES (AES128)
import Data.Bits (Bits)
import Data.Int (Int16,Int32)
import Data.List (intercalate)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import Data.NBT
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

protocolVersion :: Integer
protocolVersion = 316

-- Type Synonyms
type Short = Int16
-- Cipher, Enc, Dec
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- `HasIO` effect
type HasIO r = Member IO r
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
newtype VarInt = VarInt {unVarInt :: Int32} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show VarInt where show = show . unVarInt

newtype EntityId = EntityId {unEID :: VarInt} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show EntityId where show = (\p -> "Entity Id {" ++ p ++ "}") . show . unEID

newtype WindowId = WindowId {unWID :: Word8} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show WindowId where show = (\p -> "Window Id {" ++ p ++ "}") . show . unWID

type PlayerId = EntityId

type TransactionId = Short
type KeepAliveId = VarInt
type TPConfirmId = VarInt

-- Block id, count, damage
data Slot = EmptySlot | Slot Short Word8 Short (Maybe NbtContents) deriving Eq

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

-- Enumerations for the protocol
data Side = Server | Client

-- These are used for to select the correct parser (packet id 0 is reused in all four modes)
data ServerState = Handshaking | Playing | LoggingIn | Status

-- More gamemodes NYI
data Gamemode = Survival | Creative {- | Adventure | Spectator -} deriving Show

data Difficulty = Peaceful | Easy | Normal | Hard deriving Show


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

data ClientStatusAction = PerformRespawn | RequestStats | OpenInventory deriving Show

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

allCoords :: [BlockCoord]
allCoords = [BlockCoord (x,y,z) | y <- [0..15], z <- [0..15], x <- [0..15]]

airChunk :: Map BlockCoord BlockState
airChunk = Map.fromList [(BlockCoord (x,y,z),BlockState 0 0) | x <- [0..15], z <- [0..15], y <- [0..15]]

formatPacket :: String -> [(String,String)] -> String
formatPacket n [] = "{" ++ n ++ "}"
formatPacket n xs = flip (++) "]" . (++) ("{" ++ n ++ "} [") . intercalate " | " . map (\(name,val) -> if val == "" then name else name ++ ": " ++ val) $ xs

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
