module Encrypt where

import System.Random
import Data.List
import Data.Maybe
import Data.Semigroup
import OpenSSL.RSA
import OpenSSL.DER
import OpenSSL.X509
import OpenSSL.EVP.Cipher
import OpenSSL.EVP.Open
import OpenSSL.EVP.Base64
import OpenSSL.PEM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import System.IO.Unsafe

aes :: Cipher
aes = unsafePerformIO $ fromJust <$> getCipherByName "AES-128-CFB8"

getAKeypair :: IO RSAKeyPair
getAKeypair = generateRSAKey' 1024 17

generatePublicKey :: IO (BS.ByteString,RSAKeyPair)
generatePublicKey = do
  k <- getAKeypair
  print k
  p <- rsaCopyPublic k
  return (toDERPub p, k)

-- Apparently we have to use the encrypted ss as our IV???
decryptSS :: BS.ByteString -> RSAKeyPair -> BS.ByteString
decryptSS ss k = openBS aes ss ss k ss

decryptVT :: BS.ByteString -> RSAKeyPair -> BS.ByteString -> BS.ByteString
decryptVT ss k vt = openBS aes ss ss k vt
