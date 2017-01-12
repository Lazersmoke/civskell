{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Clientbound where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import Data.Semigroup
import Data.Word
import Data.Int
import Data.Bits
import Unsafe.Coerce

import Data

data PlayPacket = PlayPacket deriving Show

data LoginPacket
  -- Reason
  = Disconnect String
  -- Server ID, Pub Key, Verify Token
  | EncryptionRequest String BS.ByteString BS.ByteString
  -- UUID (with hyphens), Username
  | LoginSuccess String String
  -- Size threshold for compression
  | SetCompression VarInt deriving Show

data StatusPacket
  -- JSON String (for now)
  = StatusResponse String
  -- Payload (unique number obtained from client)
  | StatusPong Int64 deriving Show

instance Serialize StatusPacket where
  serialize pkt = withLength . BS.append (makeVarInt $ packetId pkt) $ case pkt of
    (StatusResponse s) -> makeVarString s
    (StatusPong l) -> makeLong l

instance PacketId StatusPacket where
  packetSide _ = Client
  packetId p = case p of 
    (StatusResponse _) -> 0x00
    (StatusPong _) -> 0x01
  packetState _ = Status

-- All login packets have their length and pktId annotated
instance Serialize LoginPacket where
  serialize pkt = withLength . BS.append (makeVarInt $ packetId pkt) $ case pkt of
    (Disconnect reason) -> makeVarString reason
    (EncryptionRequest sId p vt) -> makeVarString sId <> withLength p <> withLength vt
    (LoginSuccess uuid name) -> makeVarString uuid <> makeVarString name
    (SetCompression thresh) -> makeVarInt thresh

-- All login packets have a packet ID
instance PacketId LoginPacket where
  packetSide _ = Client
  packetId p = case p of
    (Disconnect _) -> 0x00
    (EncryptionRequest _ _ _) -> 0x01
    (LoginSuccess _ _) -> 0x02
    (SetCompression _) -> 0x03
  packetState p = case p of
    (Disconnect _) -> LoggingIn
    (EncryptionRequest _ _ _) -> LoggingIn
    (LoginSuccess _ _) -> LoggingIn
    (SetCompression _) -> LoggingIn
    

-- Annotate a BS with its length as a VarInt
withLength :: BS.ByteString -> BS.ByteString
withLength bs = makeVarInt (fromIntegral $ BS.length bs) <> bs

makeVarString :: String -> BS.ByteString
makeVarString str = makeVarInt (fromIntegral $ BS.length encoded) <> encoded
  where
    encoded = Data.ByteString.UTF8.fromString str

makeVarInt :: VarInt -> BS.ByteString
makeVarInt n = if moreAfter
  then (0b10000000 .|. writeNow) `BS.cons` makeVarInt (shiftR n 7)
  else BS.singleton writeNow
  where
    -- Write first seven bits
    writeNow = (unsafeCoerce :: VarInt -> Word8) $ n .&. 0b1111111
    moreAfter = shiftR n 7 /= 0

makeLong :: Int64 -> BS.ByteString
makeLong i = BS.pack $ map ((unsafeCoerce :: Int64 -> Word8) . shiftR i) [56,48..0]


