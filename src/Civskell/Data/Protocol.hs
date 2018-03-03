{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
-- | Serialization and deserialization of Minecraft types
module Civskell.Data.Protocol where

import qualified Data.Serialize as Cereal
import Data.NBT
import Data.Bits
import Unsafe.Coerce (unsafeCoerce)
import Numeric (readHex,showHex)
import Data.Word (Word8)
import Data.Int
import Data.String
import Data.Word
import Data.Hashable
import Data.Foldable
import GHC.Generics
import Data.Bytes.Serial
import qualified Data.Text as T
import Data.Bytes.Put
import Data.Bytes.Get
import Control.Monad
import Crypto.Cipher.AES (AES128)
import Data.Aeson (withObject,(.:),(.:?),(.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types
import qualified Data.ByteString.UTF8
import qualified GHC.Exts as Exts
import qualified Data.ByteString as BS

-- | A LegacyString is a String that is used in a Legacy Handshake packet
-- It is encoded with UTF-16BE internally
type LegacyString = BS.ByteString

-- | A Minecraft "Short" is a Haskell Int16
type Short = Int16

-- | Item ids are shorts
type ItemId = Short

-- | Block ids are shorts
type BlockId = Short

-- | A variably-sized integer, maximum 32 bits
-- Note that they are serialized differently from a normal Int32, so we need a newtype. 
newtype VarInt = VarInt {unVarInt :: Int32} deriving (Bits,Enum,Eq,Integral,Num,Ord,Real,Hashable) -- All instances are GND

-- | Deriving this gives "VarInt {unVarInt = <whatever>}"
instance Show VarInt where show = show . unVarInt

-- | VarInt format in binary is:
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

newtype ProtocolOptional a = ProtocolOptional {unProtocolOptional :: Maybe a} deriving (Eq,Show)

instance Serial a => Serial (ProtocolOptional a) where
  serialize (ProtocolOptional ma) = case ma of
    Just a -> serialize True *> serialize a
    Nothing -> serialize False
  deserialize = deserialize @Bool >>= \case
    False -> pure (ProtocolOptional Nothing)
    True -> ProtocolOptional . Just <$> deserialize @a

newtype ProtocolString = ProtocolString {unProtocolString :: String} deriving (IsString,Eq,Data.Aeson.Types.FromJSON)

instance Show ProtocolString where
  show (ProtocolString s) = show s

instance Serial ProtocolString where
  serialize (ProtocolString str) = serialize ((fromIntegral $ BS.length encoded) :: VarInt) >> putByteString encoded
    where
      encoded = Data.ByteString.UTF8.fromString str
  deserialize = deserialize @VarInt >>= \len -> ProtocolString . Data.ByteString.UTF8.toString <$> getBytes (fromIntegral len)

-- | Simple way to inject a text message into a json chat string. No additional
-- formatting or checking is done, just raw text.
jsonyText :: String -> ProtocolString
jsonyText s = ProtocolString $ "{\"text\":\"" ++ s ++ "\"}"

data ProtocolList i a = ProtocolList {unProtocolList :: [a]}

instance Exts.IsList (ProtocolList i a) where
  type Item (ProtocolList i a) = a
  fromList = ProtocolList
  toList = unProtocolList

instance (Integral i,Serial i,Serial a) => Serial (ProtocolList i a) where
  serialize (ProtocolList xs) = serialize ((fromIntegral $ length xs) :: i) >> traverse serialize xs >> pure ()
  deserialize = deserialize @i >>= \len -> ProtocolList <$> replicateM (fromIntegral len) deserialize

-- | A @'WireSlot'@ is a numerical, type-poor representation of a @'Slot'@. It has no
-- meaning or semantics until it is parsed into a proper @'Slot'@ or serialized into bytes.
data WireSlot = WireSlot (Maybe (ItemId,Word8,Short,Maybe NBT)) deriving (Eq,Show)

-- We can specify exact serialization semantics for this because it is independent of the 
-- set of actually supported items; it just represents some numbers on the wire.
-- See [the wiki.vg page](http://wiki.vg/Slot_Data) for the exact specification.
instance Serial WireSlot where
  serialize (WireSlot Nothing) = serialize @ItemId (-1)
  serialize (WireSlot (Just (iId,count,meta,mNbt))) = serialize @ItemId iId *> serialize @Word8 count *> serialize @Short meta *> serNBT mNbt
    where
      serNBT Nothing = serialize @Word8 0x00
      serNBT (Just nbt) = serialize @ProtocolNBT (ProtocolNBT nbt)
  deserialize = lookAhead (deserialize @ItemId) >>= \case
    (-1) -> pure (WireSlot Nothing)
    _ -> WireSlot . Just <$> ((,,,) <$> deserialize @ItemId <*> deserialize @Word8 <*> deserialize @Short <*> getNbt)
    where
      getNbt = lookAhead getWord8 >>= \case
        0x00 -> pure Nothing
        _ -> Just . unProtocolNBT <$> deserialize @ProtocolNBT

newtype ProtocolNBT = ProtocolNBT {unProtocolNBT :: NBT}

instance Serial ProtocolNBT where
  serialize = putByteString . Cereal.runPut . Cereal.put . unProtocolNBT
  deserialize = remaining >>= \r -> getByteString (fromIntegral r) >>= \bs -> case Cereal.runGet Cereal.get bs of
    Left e -> error e
    Right a -> pure (ProtocolNBT a)

-- | Annotate a list of serializable things with the length of the list
withListLength :: (MonadPut m,Serial s) => [s] -> m ()
withListLength ls = serialize ((fromIntegral $ length ls) :: VarInt) >> traverse_ serialize ls

-- | A ByteString that is serialized as being length annotated
newtype LengthAnnotatedByteString = LengthAnnotatedByteString {unLengthAnnotatedByteString :: BS.ByteString} deriving (Show,Eq)

instance Serial LengthAnnotatedByteString where
  serialize (LengthAnnotatedByteString bs) = serialize @VarInt (fromIntegral $ BS.length bs) *> putByteString bs
  deserialize = LengthAnnotatedByteString <$> (getByteString . fromIntegral =<< deserialize @VarInt)

-----------------
-- Named Types --
-----------------
-- * UUID

-- TODO: Investigate using a library to provide this type for seperation of concerns
-- There is no native Word128 type, so we role our own here.
newtype UUID = UUID (Word64,Word64)

instance Enum UUID where
  succ (UUID (a,b)) 
    | b /= maxBound = UUID (a, succ b)
    | otherwise = if a == maxBound 
      then error "Enum.succ{UUID}: tried to take `succ' of maxBound" 
      else UUID (succ a, minBound)
  pred (UUID (a,b)) 
    | b /= minBound = UUID (a, pred b)
    | otherwise = if a == minBound 
      then error "Enum.pred{UUID}: tried to take `pred' of minBound" 
      else UUID (pred a, maxBound) 
  toEnum n = UUID (0,fromIntegral n)
  fromEnum (UUID (_,n)) = fromIntegral n

-- This show instance should match the standard Minecraft display format.
instance Show UUID where
  show (UUID (ua,ub)) = reformat $ showHex ua (showHex ub "")
    where
      -- Add a hyphen at an index
      ins i s = let (a,b) = splitAt i s in a ++ "-" ++ b
      -- Reformat the uuid to what the client expects
      reformat = ins 8 . ins 12 . ins 16 . ins 20

-- A UUID is pretending to be a Word128, so just smoosh all the bits together
instance Serial UUID where
  serialize (UUID (a,b)) = putWord64be a >> putWord64be b
  deserialize = (UUID .) . (,) <$> getWord64be <*> getWord64be

instance Data.Aeson.Types.FromJSON UUID where
  parseJSON (Aeson.String s) = pure $ (\i -> UUID (fromInteger $ i .&. 0xFFFFFFFFFFFFFFFF,fromInteger $ shiftR i 8)) . fst . head . readHex . T.unpack $ s
  parseJSON x = Data.Aeson.Types.typeMismatch "UUID" x





-- A Nibble is a Word8 that we promise (not enforced!!!) to only use the 4 least
-- significant bits.
-- TODO: Investigate `(Bool,Bool,Bool,Bool)` or `Vect 4 Bool` as alternative.
type Nibble = Word8

-- An EncrptionCouplet holds the current internal state of the encryption
-- mechanism, including both directions' shift buffers. (Cipher,Enc,Dec)
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- Auth infrastructure is scary, I hope it doesn't break ever :3
-- Parsed form of the result of authenticating a player with Mojang
data AuthPacket = AuthPacket UUID String [AuthProperty]

-- FromJSON instances parse JSON into AuthPackets
instance Data.Aeson.Types.FromJSON AuthPacket where
  parseJSON (Aeson.Object o) = AuthPacket <$> o .: "id" <*> o .: "name" <*> o .: "properties"
  parseJSON x = Data.Aeson.Types.typeMismatch "AuthPacket" x
-- Helper type to support the arbitrary properties in auth packets
data AuthProperty = AuthProperty ProtocolString ProtocolString (ProtocolOptional ProtocolString) deriving Generic

instance Show AuthProperty where
  show (AuthProperty (ProtocolString name) (ProtocolString value) (ProtocolOptional _mSig)) = "\"" ++ name ++ "\":\"" ++ value ++ "\""

instance Serial AuthProperty where {}

instance Data.Aeson.Types.FromJSON AuthProperty where
  parseJSON = withObject "Auth Property" $ \o -> AuthProperty <$> o .: "name" <*> o .: "value" <*> (ProtocolOptional <$> o .:? "signature")

data ClientAuthResponse = ClientAuthResponse {accessToken :: String, clientToken :: String, profileInformation :: (Maybe [ClientAuthProfile],Maybe ClientAuthProfile)} deriving Show

instance Data.Aeson.Types.FromJSON ClientAuthResponse where
  parseJSON = withObject "Client Auth Response" $ \o -> ClientAuthResponse <$> o .: "accessToken" <*> o .: "clientToken" <*> ((,) <$> o .:? "availableProfiles" <*> o .:? "selectedProfile")

data ClientAuthProfile = ClientAuthProfile {clientAuthProfileId :: String, clientAuthProfileName :: String, clientAuthProfileIsLegacy :: Bool} deriving Show

instance Data.Aeson.Types.FromJSON ClientAuthProfile where
  parseJSON = withObject "Client Auth Profile" $ \o -> ClientAuthProfile <$> o .: "id" <*> o .: "name" <*> o .:? "legacy" .!= False
