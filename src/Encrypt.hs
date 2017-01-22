{-# LANGUAGE FlexibleContexts #-}
module Encrypt where

import Data.Bits
import Data.Semigroup
import qualified Crypto.PubKey.RSA as RSA
import Data.Word
import Numeric
import Crypto.Hash
import Crypto.Cipher.Types
import Crypto.Cipher.AES
import Crypto.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader

import Data

-- 128 is 128 bytes, so 1024 bit key
getAKeypair :: HasIO r => Eff r (RSA.PublicKey,RSA.PrivateKey)
getAKeypair = liftIO $ RSA.generate 128 65537 

-- Observe the cancer, but don't touch it or you'll contract it.
encodePubKey :: RSA.PublicKey -> BS.ByteString
encodePubKey k = asnSequence <> withLengthAsn (algIdentifier <> pubKeyBitstring)
  where
    -- ASN.1 object identifier for RSA keys (decoded from 1.2.840.113549.1.1.1)
    -- See section 5.9 for details: http://luca.ntop.org/Teaching/Appunti/asn1.html
    asnOIDForRSAKeys = BS.pack [0x2a,0x86,0x48,0x86,0xf7,0x0d,0x01,0x01,0x01]

    -- ASN.1 defined tags
    -- Usually you use: tag + length + data
    nullForGoodLuck = BS.singleton 0x00
    asnInt = BS.singleton 0x02
    asnBitString = BS.singleton 0x03
    asnTag = BS.singleton 0x05
    asnObjectId = BS.singleton 0x06
    asnSequence = BS.singleton 0x30

    -- ASN.1's length format helper functions
    -- for short lengths (<128) the length is a single byte
    -- for long lengths, the length is a byte with the MSB set and bits 0-7 encode
    -- how many more bytes there are. The rest of the bytes are the actual length
    --
    -- Example: 0b00001100 means Actual Length = 0b00001100 = 12
    --            ^ MSB not set
    --
    -- Example: 0b10000010 0x5e 0x67 means Actual Length = 0x5e 0x67 = 24167
    -- MSB is set ^     ^two more bytes
    lenOf bs = if BS.length bs < 128
      then BS.singleton . fromIntegral . BS.length $ bs
      else 
        let b = (intBytesRaw . fromIntegral . BS.length $ bs) 
        in (0x80 .|. (fromIntegral . BS.length $ b)) `BS.cons` b
    -- A thing and its length
    withLengthAsn a = lenOf a <> a

    algIdentifier = asnSequence <> withLengthAsn (algObjectId <> algParams)
    -- Use our precalculated magic bytes
    algObjectId = asnObjectId <> withLengthAsn asnOIDForRSAKeys
    -- 0x05 0x00 is NULL in ASN.1, and we don't have any params
    algParams = asnTag <> nullForGoodLuck

    -- idk why there is a null here
    pubKeyBitstring = asnBitString <> withLengthAsn (nullForGoodLuck <> pubKeySequence)
    pubKeySequence = asnSequence <> withLengthAsn (theModulus <> theExponent)

    -- The people who made ASN.1 like to put random nulls in for fun
    theModulus = asnInt <> withLengthAsn (nullForGoodLuck <> bytesOfModulus)
    theExponent = asnInt <> withLengthAsn bytesOfExponent

    bytesOfModulus = intBytesRaw $ RSA.public_n k
    bytesOfExponent = intBytesRaw $ RSA.public_e k

-- intBytesRaw gets the variable-length byte encoding of a number
intBytesRaw :: Integer -> BS.ByteString
intBytesRaw = BS.reverse . BS.unfoldr (\i -> if i == 0 then Nothing else Just $ (fromIntegral i, shiftR i 8))

-- unIntBytesRaw gets the Integer represented by a BS
unIntBytesRaw :: BS.ByteString -> Integer 
unIntBytesRaw = BS.foldr' (\b i -> shiftL i 8 + fromIntegral b) 0 . BS.reverse

makeEncrypter :: BS.ByteString -> AES128
makeEncrypter ss = throwCryptoError $ cipherInit ss

cfb8Encrypt :: HasEncryption r => BS.ByteString -> Eff r BS.ByteString
cfb8Encrypt = bsFoldlM magic BS.empty 
  where
    -- Does a single step (one byte) of a CFB8 encryption
    magic :: HasEncryption r => BS.ByteString -> Word8 -> Eff r BS.ByteString
    magic ds d = do 
      iv <- get
      ciph <- ask :: HasEncryption r => Eff r AES128
      -- use the MSB of the encrypted shift register to encrypt the current plaintext
      let ct = BS.head (ecbEncrypt ciph iv) `xor` d
      -- shift the new ciphertext into the shift register
      let ivFinal = BS.tail iv `BS.snoc` ct
      -- add the cipher text to the output, and return the updated shift register
      put ivFinal
      return $ ds `BS.snoc` ct

cfb8Decrypt :: HasEncryption r => BS.ByteString -> Eff r BS.ByteString
cfb8Decrypt = bsFoldlM magic BS.empty
  where
    magic :: HasEncryption r => BS.ByteString -> Word8 -> Eff r BS.ByteString
    magic ds d = do
      iv <- get
      ciph <- ask :: HasEncryption r => Eff r AES128
      let pt = BS.head (ecbEncrypt ciph iv) `xor` d
      -- snoc on cipher always
      let ivFinal = BS.tail iv `BS.snoc` d
      put ivFinal
      return (ds `BS.snoc` pt)


bsFoldlM :: Monad m => (b -> Word8 -> m b) -> b -> BS.ByteString -> m b
bsFoldlM f i bs = BS.foldr f' return bs i
  where f' x k z = f z x >>= k



--semCipherEncrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString

-- Get a login hash from a sId, shared secret, and public key
genLoginHash :: String -> BS.ByteString -> BS.ByteString -> String
genLoginHash sId ss pubKey = 
  -- bit 159 (0-indexed from right) is the negativity bit
  if testBit theHashInt 159
    -- If its negative, then do the reverse two's comp. and add a negative sign
    then "-" ++ showHex (xor (2^(160 :: Integer) - 1) $ theHashInt - 1) ""
    -- If its positive, then its already good
    else theHash
  where
    -- the hash as an Integer
    theHashInt = fst . head . (readHex :: ReadS Integer) $ theHash
    -- the hash as a String of and SHA1 hash (this is the only way to export it)
    theHash = show . hashFinalize $ hashUpdates (hashInit :: Context SHA1) [Data.ByteString.UTF8.fromString sId,ss,pubKey]
