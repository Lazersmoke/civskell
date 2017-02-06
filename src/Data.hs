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
import Data.Bits
import Data.Semigroup
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import Unsafe.Coerce
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
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

liftIO :: HasIO r => IO a -> Eff r a
liftIO = send

type HasIO = Member IO
type HasNetworking = Member Networking
type HasLogging = Member Logging
type HasPlayer = Member Player
-- Cipher, Enc, Dec
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- Enums for parsing, etc
data Side = Server | Client

data ServerState = Handshaking | Playing | LoggingIn | Status

data Position a = Position a a a deriving (Show,Eq)

-- Things that have a packet ID and a bound to side
class PacketId p where
  packetId :: p -> VarInt
  packetSide :: p -> Side
  packetState :: p -> ServerState

data Slot = EmptySlot | Slot Short Word8 Short (Maybe NbtContents) deriving Show

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

instance Serialize (Position Int) where
  serialize (Position x y z) = serialize $ 
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

data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | NormalLog deriving Eq

logg :: HasLogging r => String -> Eff r ()
logg = send . LogString NormalLog

logLevel :: HasLogging r => LogLevel -> String -> Eff r ()
logLevel l s = send (LogString l s)

runLogger :: HasIO r => Eff (Logging ': r) a -> Eff r a
runLogger (Val x) = return x
runLogger (E u q) = case decomp u of
  Right (LogString level str) -> case level of
    HexDump -> do
      liftIO (putStrLn str) 
      runLogger (qApp q ())
    ClientboundPacket -> do
      liftIO (putStrLn ("[\x1b[32mSent\x1b[0m] " ++ str))
      runLogger (qApp q ())
    ServerboundPacket -> do
      liftIO (putStrLn ("[\x1b[32mGot\x1b[0m] " ++ str))
      runLogger (qApp q ())
    ErrorLog -> do
      liftIO (putStrLn $ "[\x1b[31mERROR\x1b[0m] " ++ str) 
      runLogger (qApp q ())
    NormalLog -> do
      liftIO (putStrLn $ "[\x1b[36mCivSkell\x1b[0m] " ++ str) 
      runLogger (qApp q ())
  Left otherEffects -> E otherEffects (tsingleton (\x -> runLogger (qApp q x)))

-- Annotate a BS with its length as a VarInt
withLength :: BS.ByteString -> BS.ByteString
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) <> bs

data PlayerInfo = PlayerInfo 
  {teleportConfirmationQue :: Set.Set VarInt
  ,clientBrand :: Maybe String
  ,holdingSlot :: Short
  }

defaultPlayerInfo :: PlayerInfo
defaultPlayerInfo = PlayerInfo
  {teleportConfirmationQue = Set.empty
  ,clientBrand = Nothing
  ,holdingSlot = 0
  }

data Player a where
  -- Simple maybe state for brand
  PlayerBrand :: Player (Maybe String)
  SetPlayerBrand :: String -> Player ()
  -- Simple state for selected slot
  PlayerHolding :: Player Short
  SetPlayerHolding :: Short -> Player ()
  -- Teleport confirm que
  PlayerAddTP :: VarInt -> Player ()
  PlayerClearTP :: VarInt -> Player Bool

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

initPlayer :: Eff (Player ': r) a -> Eff r a
initPlayer = runPlayer defaultPlayerInfo

runPlayer :: PlayerInfo -> Eff (Player ': r) a -> Eff r a
runPlayer _ (Val x) = return x
runPlayer i (E u q) = case decomp u of
  -- Get the player's client brand
  Right PlayerBrand -> runPlayer i (qApp q (clientBrand i))
  -- Set the player's client brand
  Right (SetPlayerBrand b) -> runPlayer i {clientBrand = Just b} (qApp q ())
  -- Get the player's selected slot
  Right PlayerHolding -> runPlayer i (qApp q (holdingSlot i))
  -- Set the player's selected slot
  Right (SetPlayerHolding s) -> runPlayer i {holdingSlot = s} (qApp q ())
  -- Add a new tid to the que
  Right (PlayerAddTP tid) -> runPlayer i' (qApp q ())
    where
      i' = i {teleportConfirmationQue = Set.insert tid $ teleportConfirmationQue i}
  -- Check if the tid is in the que. If it is, then clear and return true, else false
  Right (PlayerClearTP tid) -> runPlayer i' (qApp q hasTid)
    where 
      i' = i {teleportConfirmationQue = Set.delete tid $ teleportConfirmationQue i}
      hasTid = Set.member tid $ teleportConfirmationQue i
  -- Not our turn
  Left otherEffects -> E otherEffects (tsingleton (\x -> runPlayer i (qApp q x)))
