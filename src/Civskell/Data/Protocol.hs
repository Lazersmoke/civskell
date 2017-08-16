{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module provides types relevent to the Minecraft Protocol
module Civskell.Data.Protocol where

import Data.Bytes.Serial
import Data.Bits
import Data.Word (Word8)

import Civskell.Data.Types

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


