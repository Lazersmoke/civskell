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
module Data where

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
      writeDat (_:[]) = error "Bad chunksection block list size"

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

data Logging a where
  LogString :: LogLevel -> String -> Logging ()

data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | VerboseLog | NormalLog deriving Eq

logg :: HasLogging r => String -> Eff r ()
logg = send . LogString NormalLog

logLevel :: HasLogging r => LogLevel -> String -> Eff r ()
logLevel l s = send (LogString l s)

runLogger :: HasIO r => Eff (Logging ': r) a -> Eff r a
runLogger (Pure x) = return x
runLogger (Eff u q) = case u of
  Inject (LogString level str) -> case level of
    HexDump -> do
      --liftIO (putStrLn str) 
      runLogger (runTCQ q ())
    ClientboundPacket -> do
      liftIO (putStrLn ("[\x1b[32mSent\x1b[0m] " ++ str))
      runLogger (runTCQ q ())
    ServerboundPacket -> do
      liftIO (putStrLn ("[\x1b[32mGot\x1b[0m] " ++ str))
      runLogger (runTCQ q ())
    ErrorLog -> do
      liftIO (putStrLn $ "[\x1b[31mERROR\x1b[0m] " ++ str) 
      runLogger (runTCQ q ())
    VerboseLog -> do
      --liftIO (putStrLn $ "[\x1b[36mCivSkell/Verbose\x1b[0m] " ++ str) 
      runLogger (runTCQ q ())
    NormalLog -> do
      liftIO (putStrLn $ "[\x1b[36mCivSkell\x1b[0m] " ++ str) 
      runLogger (runTCQ q ())
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runLogger (runTCQ q x)))

-- Annotate a BS with its length as a VarInt
withLength :: BS.ByteString -> BS.ByteString
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) <> bs

withListLength :: Serialize s => [s] -> BS.ByteString
withListLength ls = serialize ((fromIntegral $ length ls) :: VarInt) <> BS.concat (map serialize ls)

data PlayerInfo = PlayerInfo 
  {teleportConfirmationQue :: Set.Set VarInt
  ,keepAliveQue :: Set.Set VarInt
  ,clientBrand :: Maybe String
  ,holdingSlot :: Short
  ,playerPosition :: (Double,Double,Double)
  ,viewAngle :: (Float,Float)
  ,gameMode :: Gamemode
  }

defaultPlayerInfo :: PlayerInfo
defaultPlayerInfo = PlayerInfo
  {teleportConfirmationQue = Set.empty
  ,keepAliveQue = Set.empty
  ,clientBrand = Nothing
  ,holdingSlot = 0
  ,playerPosition = (0,0,0)
  ,viewAngle = (0,0)
  ,gameMode = Creative
  }

data Player a where
  -- Simple maybe state for brand
  PlayerBrand :: Player (Maybe String)
  SetPlayerBrand :: String -> Player ()
  -- Simple state for selected slot
  PlayerHolding :: Player Short
  SetPlayerHolding :: Short -> Player ()
  -- Simple state for Gamemode
  PlayerGamemode :: Player Gamemode
  SetPlayerGamemode :: Gamemode -> Player ()
  -- Teleport confirm que
  PlayerAddTP :: VarInt -> Player ()
  PlayerClearTP :: VarInt -> Player Bool
  -- Keep Alive confirm que
  PlayerAddKeepAlive :: VarInt -> Player ()
  PlayerClearKeepAlive :: VarInt -> Player Bool
  -- Position and Look
  GetPlayerPosition :: Player (Double,Double,Double)
  SetPlayerPosition :: (Double,Double,Double) -> Player ()
  GetPlayerViewAngle :: Player (Float,Float)
  SetPlayerViewAngle :: (Float,Float) -> Player ()

getBrand :: HasPlayer r => Eff r (Maybe String)
getBrand = send PlayerBrand

setBrand :: HasPlayer r => String -> Eff r ()
setBrand = send . SetPlayerBrand

getHolding :: HasPlayer r => Eff r Short
getHolding = send PlayerHolding

setHolding :: HasPlayer r => Short -> Eff r ()
setHolding = send . SetPlayerHolding

pendTeleport :: HasPlayer r => VarInt -> Eff r ()
pendTeleport = send . PlayerAddTP

clearTeleport :: HasPlayer r => VarInt -> Eff r Bool
clearTeleport = send . PlayerClearTP

pendKeepAlive :: HasPlayer r => VarInt -> Eff r ()
pendKeepAlive = send . PlayerAddKeepAlive

clearKeepAlive :: HasPlayer r => VarInt -> Eff r Bool
clearKeepAlive = send . PlayerClearKeepAlive

initPlayer :: HasWorld r => Eff (Player ': r) a -> Eff r a
initPlayer = (newPlayer >>=) . flip runPlayer

getGamemode :: HasPlayer r => Eff r Gamemode
getGamemode = send PlayerGamemode

setGamemode :: HasPlayer r => Gamemode -> Eff r ()
setGamemode = send . SetPlayerGamemode

setPlayerPos :: HasPlayer r => (Double,Double,Double) -> Eff r ()
setPlayerPos = send . SetPlayerPosition

getPlayerPos :: HasPlayer r => Eff r (Double,Double,Double)
getPlayerPos = send GetPlayerPosition

setPlayerViewAngle :: HasPlayer r => (Float,Float) -> Eff r ()
setPlayerViewAngle = send . SetPlayerViewAngle

getPlayerViewAngle :: HasPlayer r => Eff r (Float,Float)
getPlayerViewAngle = send GetPlayerViewAngle

runPlayer :: HasWorld r => PlayerId -> Eff (Player ': r) a -> Eff r a
runPlayer _ (Pure x) = return x
runPlayer i (Eff u q) = getPlayer i >>= \p -> case u of
  -- Get the player's client brand
  Inject PlayerBrand -> runPlayer i (runTCQ q (clientBrand p))
  -- Set the player's client brand
  Inject (SetPlayerBrand b) -> setPlayer i p {clientBrand = Just b} >> runPlayer i (runTCQ q ())
  -- Get the player's selected slot
  Inject PlayerGamemode -> runPlayer i (runTCQ q (gameMode p))
  -- Set the player's selected slot
  Inject (SetPlayerGamemode g) -> setPlayer i p {gameMode = g} >> runPlayer i (runTCQ q ())
  -- Get the player's selected slot
  Inject PlayerHolding -> runPlayer i (runTCQ q (holdingSlot p))
  -- Set the player's selected slot
  Inject (SetPlayerHolding s) -> setPlayer i p {holdingSlot = s} >> runPlayer i (runTCQ q ())
  -- Add a new tid to the que
  Inject (PlayerAddTP tid) -> do
    setPlayer i p {teleportConfirmationQue = Set.insert tid $ teleportConfirmationQue p}
    runPlayer i (runTCQ q ())
  -- Check if the tid is in the que. If it is, then clear and return true, else false
  Inject (PlayerClearTP tid) -> do
    setPlayer i p {teleportConfirmationQue = Set.delete tid $ teleportConfirmationQue p}
    runPlayer i (runTCQ q $ Set.member tid $ teleportConfirmationQue p)
  -- Add a new keep alive id to the que
  Inject (PlayerAddKeepAlive k) -> do
    setPlayer i p {keepAliveQue = Set.insert k $ keepAliveQue p}
    runPlayer i (runTCQ q ())
  -- Check if the keep alive id is in the que. If it is, then clear and return true, else false
  Inject (PlayerClearKeepAlive k) -> do
    setPlayer i p {keepAliveQue = Set.delete k $ keepAliveQue p} 
    runPlayer i (runTCQ q $ Set.member k $ keepAliveQue p)
  Inject GetPlayerPosition -> runPlayer i . runTCQ q . playerPosition $ p
  Inject (SetPlayerPosition xyz) -> setPlayer i p {playerPosition = xyz} >> runPlayer i (runTCQ q ())
  Inject GetPlayerViewAngle -> runPlayer i . runTCQ q . viewAngle $ p
  Inject (SetPlayerViewAngle yp) -> setPlayer i p {viewAngle = yp} >> runPlayer i (runTCQ q ())
  -- Not our turn
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runPlayer i (runTCQ q x)))

data WorldData = WorldData {chunks :: Map ChunkCoord ChunkSection, players :: Map PlayerId PlayerInfo, nextPlayerId :: PlayerId}

data World a where
  SetChunk :: ChunkSection -> ChunkCoord -> World ()
  GetChunk :: ChunkCoord -> World ChunkSection
  NewPlayer :: World PlayerId
  SetPlayer :: PlayerId -> PlayerInfo -> World ()
  GetPlayer :: PlayerId -> World PlayerInfo
  AllPlayers :: World [PlayerInfo]

getChunk :: Member World r => ChunkCoord -> Eff r ChunkSection
getChunk = send . GetChunk

getBlock :: Member World r => BlockCoord -> Eff r BlockState
getBlock b = blockInChunk (blockToRelative b) <$> getChunk (blockToChunk b)

setChunk :: Member World r => ChunkSection -> ChunkCoord -> Eff r ()
setChunk c cc = send $ SetChunk c cc

setBlock :: Member World r => BlockCoord -> BlockState -> Eff r ()
setBlock b b' = getChunk (blockToChunk b) >>= \(ChunkSection bs) -> setChunk (ChunkSection $ Map.insert (blockToRelative b) b' bs) (blockToChunk b)

removeBlock :: Member World r => BlockCoord -> Eff r ()
removeBlock bc = getChunk (blockToChunk bc) >>= \(ChunkSection bs) -> setChunk (ChunkSection $ Map.delete (blockToRelative bc) bs) (blockToChunk bc)

setPlayer :: Member World r => PlayerId -> PlayerInfo -> Eff r ()
setPlayer i p = send $ SetPlayer i p

getPlayer :: Member World r => PlayerId -> Eff r PlayerInfo
getPlayer = send . GetPlayer

newPlayer :: Member World r => Eff r PlayerId
newPlayer = send NewPlayer

allPlayers :: Member World r => Eff r [PlayerInfo]
allPlayers = send AllPlayers

initWorld :: Eff (World ': r) a -> Eff r a
initWorld = runWorld WorldData {chunks = Map.empty,players = Map.empty, nextPlayerId = 0}

runWorld :: WorldData -> Eff (World ': r) a -> Eff r a
runWorld _ (Pure x) = Pure x
runWorld w (Eff u q) = case u of
  Inject (GetChunk chunk) -> case Map.lookup chunk (chunks w) of
    Just theChunk -> runWorld w (runTCQ q theChunk)
    Nothing -> error $ "Tried to load non-existant chunk section: " ++ show chunk
  Inject (SetChunk c' cc) -> runWorld w {chunks = Map.insert cc c' (chunks w)} (runTCQ q ())
  Inject NewPlayer -> runWorld w {nextPlayerId = succ (nextPlayerId w)} (runTCQ q (nextPlayerId w))
  Inject (GetPlayer i) -> runWorld w (runTCQ q (Map.findWithDefault defaultPlayerInfo i (players w)))
  Inject (SetPlayer i p) -> runWorld w {players = Map.insert i p (players w)} (runTCQ q ())
  Inject AllPlayers -> runWorld w (runTCQ q (Map.elems (players w)))
  Weaken u' -> Eff u' (Singleton (runWorld w . runTCQ q))
