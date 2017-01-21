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
module Data where

import Data.Int
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import Unsafe.Coerce
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import System.IO (Handle)
import Crypto.Cipher.AES

-- VarInt's are parsed into Int32's
type VarInt = Int32

liftIO :: HasIO r => IO a -> Eff r a
liftIO = send

type HasIO = Member IO
type HasNetworking = Member Networking
type HasLogging = Member Logging
type HasEncryption = Member (State (AES128,BS.ByteString))

-- Enums for parsing, etc
data Side = Server | Client
data ServerState = Handshaking | Playing | LoggingIn | Status

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

data Networking a where
  GetFromNetwork :: Int -> Networking BS.ByteString
  PutIntoNetwork :: BS.ByteString -> Networking ()

rGet :: HasNetworking n => Int -> Eff n BS.ByteString
rGet = send . GetFromNetwork

rPut :: HasNetworking n => BS.ByteString -> Eff n ()
rPut = send . PutIntoNetwork

runNetworking :: HasIO r => Handle -> Eff (Networking ': r) a -> Eff r a
runNetworking _ (Val x) = return x
runNetworking hdl (E u q) = case decomp u of 
  Right now -> match now 
  Left restOfU -> E restOfU (tsingleton (\x -> runNetworking hdl (qApp q x)))
  where
    match (GetFromNetwork len) = do
      dat <- liftIO (BS.hGet hdl len)
      runNetworking hdl (qApp q dat)
    match (PutIntoNetwork bs) = do
      liftIO (BS.hPut hdl bs)
      runNetworking hdl (qApp q ())

data Logging a where
  LogString :: String -> Logging ()

logg :: HasLogging r => String -> Eff r ()
logg = send . LogString

runLogger :: HasIO r => Eff (Logging ': r) a -> Eff r a
runLogger (Val x) = return x
runLogger (E u q) = case decomp u of
  Right (LogString str) -> liftIO (putStrLn str) >> runLogger (qApp q ())
  Left otherEffects -> E otherEffects (tsingleton (\x -> runLogger (qApp q x)))
