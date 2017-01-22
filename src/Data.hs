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
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import Unsafe.Coerce
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import Crypto.Cipher.AES

-- VarInt's are parsed into Int32's
newtype VarInt = VarInt {unVarInt :: Int32} deriving (Num,Show,Bits,Eq,Enum,Integral,Real,Ord)
type Short = Int16

liftIO :: HasIO r => IO a -> Eff r a
liftIO = send

type HasIO = Member IO
type HasNetworking = Member Networking
type HasLogging = Member Logging
type HasEncryption r = (Member (Reader AES128) r, Member (State BS.ByteString) r)

-- Enums for parsing, etc
data Side = Server | Client
data ServerState = Handshaking | Playing | LoggingIn | Status

data Position = Position Int Int Int deriving (Show,Eq)
 

-- Things that have a packet ID and a bound to side
class PacketId p where
  packetId :: p -> VarInt
  packetSide :: p -> Side
  packetState :: p -> ServerState

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

instance Serialize Int32 where
  serialize i = BS.pack $ map ((unsafeCoerce :: Int32 -> Word8) . shiftR i) [24,16..0]

instance Serialize Float where
  serialize i = serialize $ (unsafeCoerce :: Float -> Int32) i

instance Serialize Int64 where
  serialize i = BS.pack $ map ((unsafeCoerce :: Int64 -> Word8) . shiftR i) [56,48..0]

instance Serialize Position where
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
  GetFromNetwork :: Int -> Networking BS.ByteString
  PutIntoNetwork :: BS.ByteString -> Networking ()

rGet :: HasNetworking n => Int -> Eff n BS.ByteString
rGet = send . GetFromNetwork

rPut :: HasNetworking n => BS.ByteString -> Eff n ()
rPut = send . PutIntoNetwork

setCompression :: HasNetworking n => Maybe VarInt -> Eff n ()
setCompression = send . SetCompressionLevel

addCompression :: HasNetworking n => BS.ByteString -> Eff n BS.ByteString
addCompression = send . AddCompression

removeCompression :: HasNetworking n => BS.ByteString -> Eff n BS.ByteString
removeCompression = send . RemoveCompression

data Logging a where
  LogString :: String -> Logging ()


logg :: HasLogging r => String -> Eff r ()
logg = send . LogString

runLogger :: HasIO r => Eff (Logging ': r) a -> Eff r a
runLogger (Val x) = return x
runLogger (E u q) = case decomp u of
  Right (LogString str) -> liftIO (putStrLn str) >> runLogger (qApp q ())
  Left otherEffects -> E otherEffects (tsingleton (\x -> runLogger (qApp q x)))

-- Annotate a BS with its length as a VarInt
withLength :: BS.ByteString -> BS.ByteString
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) <> bs
