{-# LANGUAGE LambdaCase #-}
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
  ,defaultPlayerInfo
  ,protocolVersion
  -- Helper functions
  ,blockInChunk
  ,blockToChunk
  ,blockToRelative
  ,blockOnSide
  ,showPacket
  ,jsonyText
  -- Type synonyms
  ,EncryptionCouplet
  ,HasIO
  ,Short
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
  ,ClientStatusAction(..)
  ,GameStateChange(..)
  ,InventoryClickMode(..)
  ,PlayerDigAction(..)
  ,PlayerEntityAction(..)
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
  ,PlayerInfo(..)
  ,Slot(..)
  -- Aux Enums
  ,Difficulty(..)
  ,Gamemode(..)
  ,Hand(..)
  ,MoveMode(..)
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
  ,HasLogging
  ,Logging(..)
  ,LogLevel(..)
  -- Network
  ,HasNetworking
  ,Networking(..)
  -- Packet
  ,Packet(..)
  ,ClientPacket(..)
  ,ServerPacket

  ,Serialize(..)
  ) where

import Control.Eff
import Crypto.Cipher.AES (AES128)
import Data.SuchThat
import Data.Bits (Bits)
import Data.Int (Int16,Int32)
import Data.List (intercalate)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import Data.NBT
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map
import Data.Attoparsec.ByteString (Parser)
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
data BlockBreak = InProgress Word8 | DoneBreaking

-- A block coordinate, not an entity position. Ord is derivied for use in maps.
data Block (r :: Relativity) = Block (Int,Int,Int) deriving (Eq,Ord)
type BlockCoord = Block 'Absolute
type BlockOffset = Block 'Relative

instance Show BlockCoord where
  show (Block (x,y,z)) = "(Block)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

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
data ChunkSection = ChunkSection (Map BlockOffset BlockState)

-- Derived instance is really long
instance Show ChunkSection where
  show _ = "<Chunk Section>"

-- Enumerations for the protocol
data Side = Server | Client

-- Kind for BlockCoord
data Relativity = Relative | Absolute

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

allCoords :: [BlockOffset]
allCoords = [Block (x,y,z) | y <- [0..15], z <- [0..15], x <- [0..15]]

airChunk :: Map BlockOffset BlockState
airChunk = Map.fromList [(Block (x,y,z),BlockState 0 0) | x <- [0..15], z <- [0..15], y <- [0..15]]

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

type HasPlayer r = Member Player r

data Player a where
  -- Simple maybe state for brand
  PlayerBrand :: Player String
  SetPlayerBrand :: String -> Player ()
  -- Simple maybe state for username
  PlayerName :: Player String
  SetPlayerName :: String -> Player ()
  -- Simple maybe state for UUID
  PlayerUUID :: Player String
  SetPlayerUUID :: String -> Player ()
  -- Simple state for selected slot
  PlayerHolding :: Player Short
  SetPlayerHolding :: Short -> Player ()
  -- Simple state for Gamemode
  PlayerGamemode :: Player Gamemode
  SetPlayerGamemode :: Gamemode -> Player ()
  -- Teleport confirm que
  PlayerAddTP :: (Double,Double,Double) -> (Float,Float) -> Word8 -> Player ()
  PlayerClearTP :: VarInt -> Player Bool
  -- Position and Look
  GetPlayerPosition :: Player (Double,Double,Double)
  SetPlayerPosition :: (Double,Double,Double) -> Player ()
  GetPlayerViewAngle :: Player (Float,Float)
  SetPlayerViewAngle :: (Float,Float) -> Player ()
  -- Player Inventory
  GetPlayerSlot :: Short -> Player Slot
  SetPlayerSlot :: Short -> Slot -> Player ()
  -- Breaking Blocks
  StartBreaking :: BlockCoord -> Player ()
  StopBreaking :: BlockCoord -> Player ()
  -- Sprinting and Sneaking
  SetMoveMode :: MoveMode -> Player ()
  GetMoveMode :: Player MoveMode
  -- Flush Inbox
  FlushInbox :: Player ()
  -- Log with player name as tag
  LogPlayerName :: String -> Player ()

type HasLogging r = Member Logging r

data Logging a where
  LogString :: LogLevel -> String -> Logging ()

data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | VerboseLog | TaggedLog String | NormalLog deriving Eq

data WorldData = WorldData {chunks :: Map ChunkCoord ChunkSection, players :: Map PlayerId PlayerInfo, nextPlayerId :: PlayerId, broadcastLog :: [([PlayerId],ForAny ClientPacket)]}

type HasWorld r = Member World r

data World a where
  GetChunk :: ChunkCoord -> World ChunkSection
  SetBlock :: BlockState -> BlockCoord -> World ()
  SetChunk :: ChunkSection -> ChunkCoord -> World ()
  SetColumn :: [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> World ()
  NewPlayer :: World PlayerId
  SetPlayer :: PlayerId -> PlayerInfo -> World ()
  GetPlayer :: PlayerId -> World PlayerInfo
  AllPlayers :: World [PlayerInfo]
  ForallPlayers :: (PlayerInfo -> PlayerInfo) -> World ()
  InboxForPlayer :: PlayerId -> World [ForAny ClientPacket]
  BroadcastPacket :: ForAny ClientPacket -> World ()

-- Things that can be serialized into a BS for the network
class Serialize s where
  serialize :: s -> BS.ByteString

class Packet p where
  type PacketSide p :: Side
  type PacketState p :: ServerState
  packetPretty :: p -> [(String,String)]
  packetName :: String
  packetId :: VarInt
  onPacket :: (HasLogging r,HasPlayer r,HasWorld r,HasNetworking r) => p -> Eff r ()
  onPacket _ = Pure ()
  parsePacket :: Parser p

class (Packet p,PacketSide p ~ 'Server,s ~ PacketState p) => SP s p where {}
instance (Packet p,PacketSide p ~ 'Server,s ~ PacketState p) => SP s p where {}

class (Packet p,PacketSide p ~ 'Client,s ~ PacketState p) => CP s p where {}
instance (Packet p,PacketSide p ~ 'Client,s ~ PacketState p) => CP s p where {}

showPacket :: forall p. Packet p => p -> String
showPacket pkt = formatPacket (packetName @p) (packetPretty pkt)

type ServerPacket s = SuchThatStar '[Packet,SP s]

--data ServerPacket s = forall p. (Packet p,PacketSide p ~ 'Server,PacketState p ~ s) => ServerPacket p
instance Show (ServerPacket s) where show (SuchThatStar p) = showPacket p

--data ForAny (p :: k -> *) = forall s. ForAny (p s)

newtype ClientPacket s = ClientPacket (SuchThatStar '[Packet,CP s,Serialize])
--instance Show (ClientPacket s) where show (ClientPacket p) = showPacket p
--instance Serialize (ClientPacket s) where serialize (ClientPacket p) = serialize p

type HasNetworking r = Member Networking r

data Networking a where
  SetCompressionLevel :: Maybe VarInt -> Networking ()
  AddCompression :: BS.ByteString -> Networking BS.ByteString
  RemoveCompression :: BS.ByteString -> Networking BS.ByteString
  SetupEncryption :: EncryptionCouplet -> Networking ()
  GetFromNetwork :: Int -> Networking BS.ByteString
  PutIntoNetwork :: BS.ByteString -> Networking ()
  IsPacketReady :: Networking Bool


