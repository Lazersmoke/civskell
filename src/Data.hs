{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BinaryLiterals #-}
module Data where

import Data.Int
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import Unsafe.Coerce
import Control.Eff
import Control.Eff.Lift

-- VarInt's are parsed into Int32's
type VarInt = Int32

type HasIO = SetMember Lift (Lift IO)

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
