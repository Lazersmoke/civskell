{-# LANGUAGE FlexibleContexts #-}
module Civskell.Tech.Encrypt 
  (getAKeypair
  ,encodePubKey
  ,checkVTandSS
  ,genLoginHash
  ,cfb8Encrypt,cfb8Decrypt
  ,makeEncrypter
  ) where

import Control.Eff (Eff,send)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (ecbEncrypt,cipherInit)
import Crypto.Error (throwCryptoError)
import Crypto.Hash (SHA1,Context,hashInit,hashUpdates,hashFinalize)
import Data.Bits
import Data.Semigroup ((<>))
import Numeric (showHex,readHex)
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8

import Civskell.Data.Types

-- 128 is 128 bytes, so 1024 bit key
-- We need to specify the type here because RSA.generate works in any MonadRandom
getAKeypair :: HasIO r => Eff r (RSA.PublicKey,RSA.PrivateKey)
getAKeypair = send (RSA.generate 128 65537 :: IO (RSA.PublicKey,RSA.PrivateKey))

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
--unIntBytesRaw :: BS.ByteString -> Integer
--unIntBytesRaw = BS.foldr' (\b i -> shiftL i 8 + fromIntegral b) 0 . BS.reverse

makeEncrypter :: BS.ByteString -> AES128
makeEncrypter ss = throwCryptoError $ cipherInit ss

cfb8Encrypt :: AES128 -> BS.ByteString -> BS.ByteString -> (BS.ByteString,BS.ByteString)
cfb8Encrypt c i = BS.foldl magic (BS.empty,i)
  where
    -- Does a single step (one byte) of a CFB8 encryption
    -- add the cipher text to the output, and return the updated shift register
    magic (ds,iv) d = (ds `BS.snoc` ct,ivFinal)
      where
        -- use the MSB of the encrypted shift register to encrypt the current plaintext
        ct = BS.head (ecbEncrypt c iv) `xor` d
        -- shift the new ciphertext into the shift register
        ivFinal = BS.tail iv `BS.snoc` ct

cfb8Decrypt :: AES128 -> BS.ByteString -> BS.ByteString -> (BS.ByteString,BS.ByteString)
cfb8Decrypt c i = BS.foldl magic (BS.empty,i)
  where
    magic (ds,iv) d = (ds `BS.snoc` pt,ivFinal)
      where
        pt = BS.head (ecbEncrypt c iv) `xor` d
        -- snoc on cipher always
        ivFinal = BS.tail iv `BS.snoc` d

--bsFoldlM :: Monad m => (b -> Word8 -> m b) -> b -> BS.ByteString -> m b
--bsFoldlM f i bs = BS.foldr f' return bs i
  --where f' x k z = f z x >>= k

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

checkVTandSS :: RSA.PrivateKey -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
checkVTandSS priv vtFromClient ssFromClient actualVT = do
  -- Try to decrypt their vt response
  case RSA.decrypt Nothing priv vtFromClient of
    -- If it fails, print the error
    Left e -> Left $ "Failed to parse Verify Token: " ++ show e
    -- If it decrypts properly, make sure it matches the original vt
    Right vtHopefully -> if vtHopefully /= actualVT
      -- If it isn't, error out with a Left
      then Left "Invalid Verify Token"
      -- If the verify token was ok, then decrypt the ss
      else case RSA.decrypt Nothing priv ssFromClient of
        -- If it fails to decrypt, error out with a Left
        Left e -> Left $ "Failed to decrpyt Shared Secret: " ++ show e
        -- If everything worked properly, then return the shared secret
        Right ss -> Right ss


