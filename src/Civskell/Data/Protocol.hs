{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
-- This module provides types relevent to the Minecraft Protocol
module Civskell.Data.Protocol where

import Data.Bits
import Unsafe.Coerce (unsafeCoerce)
import Data.Word (Word8)
import Data.Int
import Data.Hashable
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import qualified Data.ByteString as BS

-- These are types that go by another/more common name in the 
-- Minecraft Protocol, and are easier to understand with synonyms
--
-- A LegacyString is a String that is used in a Legacy Handshake packet
-- It is encoded with UTF-16BE internally
type LegacyString = BS.ByteString

-- A Minecraft "Short" is a Haskell Int16
type Short = Int16

-- A variably-sized integer, maximum 32 bits
-- Note that they are serialized differently from a normal Int32, so we need a newtype. 
newtype VarInt = VarInt {unVarInt :: Int32} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real,Hashable) -- All instances are GND

-- Deriving this gives "VarInt {unVarInt = <whatever>}"
instance Show VarInt where show = show . unVarInt

-- VarInt format in binary is:
--   
--   iddddddd iddddddd ...
-- 
-- Where when `i` is set, there is still another byte to be read. All the `d`s
-- from all the bytes are concatenated without spacing to form the actual number.
instance Serial VarInt where
  serialize n = if n' /= 0
    -- If there are more, set the msb and recurse
    then putWord8 (0b10000000 .|. writeNow) >> serialize n'
    -- Otherwise, just use this one
    else putWord8 writeNow
    where
      -- Write first seven bits
      writeNow = (unsafeCoerce :: VarInt -> Word8) $ n .&. 0b01111111
      n' = shiftR n 7
  -- TODO: verify this
  deserialize = do
    b <- getWord8
    if testBit b 7 
      then (\rest -> (unsafeCoerce $ b .&. 0b01111111) .|. shiftL rest 7) <$> deserialize @VarInt
      else pure (unsafeCoerce $ b .&. 0b01111111)

-- TODO: Investigate GND for Serial here: it has to do with some role nonsense
-- EntityId's are distinguished VarInt's. Note that the `Serial` instance here is
-- GND, not Generics. (although it could be, and it would work the same)
newtype EntityId = EntityId {unEID :: VarInt} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show EntityId where show = (\p -> "Entity Id {" ++ p ++ "}") . show . unEID
instance Serial EntityId where
  serialize = serialize . unEID
  deserialize = EntityId <$> deserialize

-- WindowId's are distinguished Word8's. `Serial` is GND, not Generics.
newtype WindowId = WindowId {unWID :: Word8} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show WindowId where show = (\p -> "Window Id {" ++ p ++ "}") . show . unWID
instance Serial WindowId where
  serialize = serialize . unWID
  deserialize = WindowId <$> deserialize

-- PlayerId's are *undistinguished* EntityId's because they are not different in
-- any way except the probably represent players instead of mobs. This is made
-- explicit to prevent additional confusion.
type PlayerId = EntityId

-- These are undistinguished because they are used infrequently. 
-- TODO: Investigate if these should actually be newtypes
type TransactionId = Short
type KeepAliveId = VarInt
type TPConfirmId = VarInt


