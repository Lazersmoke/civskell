{-# LANGUAGE MultiParamTypeClasses #-}
module Data where

import Data.Int
import qualified Data.ByteString as BS (ByteString)

type VarInt = Int32

data Side = Server | Client
data State = Handshaking | Playing | LoggingIn | Status

-- Things that have a packet ID and a bound to side
class PacketId p where
  packetId :: p -> VarInt
  packetSide :: p -> Side
  packetState :: p -> State

-- Things that can be parsed from a BS from the network
--class Parseable p where
  --parser :: p -> Parser p

-- Things that can be serialized into a BS for the network
class Show s => Serialize s where
  serialize :: s -> BS.ByteString
