{-# LANGUAGE MultiParamTypeClasses #-}
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

type VarInt = Int32

data Side = Server | Client
data ServerState = Handshaking | Playing | LoggingIn | Status

-- Things that have a packet ID and a bound to side
class PacketId p where
  packetId :: p -> VarInt
  packetSide :: p -> Side
  packetState :: p -> ServerState

-- Things that can be parsed from a BS from the network
--class Parseable p where
  --parser :: p -> Parser p

-- Things that can be serialized into a BS for the network
class Show s => Serialize s where
  serialize :: s -> BS.ByteString

-- Instances for Haskell types
instance Serialize String where
  serialize str = serialize ((fromIntegral $ BS.length encoded) :: VarInt) `BS.append` encoded
    where
      encoded = Data.ByteString.UTF8.fromString str

instance Serialize VarInt where
  serialize n = if moreAfter
    then (0b10000000 .|. writeNow) `BS.cons` serialize (shiftR n 7)
    else BS.singleton writeNow
    where
      -- Write first seven bits
      writeNow = (unsafeCoerce :: VarInt -> Word8) $ n .&. 0b1111111
      moreAfter = shiftR n 7 /= 0
