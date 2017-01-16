module Encrypt where

import System.Random
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Word
import Data.Bits
import OpenSSL.RSA
import OpenSSL.DER
import OpenSSL.X509
import OpenSSL.EVP.Cipher
import OpenSSL.EVP.Open
import OpenSSL.EVP.Base64
import OpenSSL.PEM
import Data.Time.Clock
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Hexdump

import System.IO.Unsafe
import Unsafe.Coerce

import Clientbound 
import Data

aes :: Cipher
aes = unsafePerformIO $ fromJust <$> getCipherByName "AES-128-CFB8"

getAKeypair :: IO RSAKeyPair
getAKeypair = generateRSAKey' 1024 65537 

generatePublicKey :: IO (BS.ByteString,RSAKeyPair)
generatePublicKey = do
  k <- getAKeypair
  print $ rsaE k
  putStrLn . prettyHex $ intBytesRaw $ rsaE k
  putStrLn . prettyHex . BS.singleton . fromIntegral . BS.length . intBytesRaw . rsaE $ k
  return (encodePubKey k, k)

-- Apparently we have to use the encrypted ss as our IV???
decryptSS :: BS.ByteString -> RSAKeyPair -> BS.ByteString
decryptSS ss k = openBS aes ss ss k ss

decryptVT :: BS.ByteString -> RSAKeyPair -> BS.ByteString -> BS.ByteString
decryptVT ss k vt = openBS aes ss ss k vt

-- Observe the cancer, but don't touch it or you'll contract it.
encodePubKey :: RSAKeyPair -> BS.ByteString
encodePubKey k = asnSequence <> withLength (algIdentifier <> pubKeyBitstring)
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
    withLength a = lenOf a <> a

    algIdentifier = asnSequence <> withLength (algObjectId <> algParams)
    -- Use our precalculated magic bytes
    algObjectId = asnObjectId <> withLength asnOIDForRSAKeys
    -- 0x05 0x00 is NULL in ASN.1, and we don't have any params
    algParams = asnTag <> nullForGoodLuck

    -- idk why there is a null here
    pubKeyBitstring = asnBitString <> withLength (nullForGoodLuck <> pubKeySequence)
    pubKeySequence = asnSequence <> withLength (theModulus <> theExponent)

    -- The people who made ASN.1 like to put random nulls in for fun
    theModulus = asnInt <> withLength (nullForGoodLuck <> bytesOfModulus)
    theExponent = asnInt <> withLength bytesOfExponent

    bytesOfModulus = intBytesRaw $ rsaN k
    bytesOfExponent = intBytesRaw $ rsaE k

-- intBytesRaw gets the variable-length byte encoding of a number
intBytesRaw :: Integer -> BS.ByteString
intBytesRaw = BS.reverse . BS.unfoldr (\i -> if i == 0 then Nothing else Just $ (fromIntegral i, shiftR i 8))


