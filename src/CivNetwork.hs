{-# LANGUAGE FlexibleContexts #-}
module CivNetwork where

import Data.Bits
import Data.Word
import Hexdump
import System.IO
import Text.Parsec
import Unsafe.Coerce
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client
import Text.Parsec.ByteString
import qualified Text.Parsec.Char as C
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict

import Data
import ParseBS
import qualified Serverbound as Server

rPut :: (HasIO r,Member (Reader Handle) r) => BS.ByteString -> Eff r ()
rPut bs = do
  hdl <- ask
  lift $ BS.hPut hdl bs

rGet :: (HasIO r,Member (Reader Handle) r) => Int -> Eff r BS.ByteString
rGet size = do
  hdl <- ask
  lift $ BS.hGet hdl size

-- Send something serializeable over the network
sendSerial :: (HasIO r,Member (Reader Handle) r,Serialize s) => s -> Eff r ()
sendSerial s = do
  -- Log its hex dump
  lift . putStrLn $ "Sending: "
  lift . putStrLn $ indentedHex (serialize s)
  -- Send it
  rPut $ serialize s

-- Send something serializeable, but encrypt it using the given cipher and iv
sendSerialEnc :: (HasIO r, Member (Reader Handle) r,Serialize s) => (AES128,BS.ByteString) -> s -> Eff r BS.ByteString
sendSerialEnc (ciph,iv) s = do
  -- Serialize the data, then unpack into a list of bytes
  let dat = BS.unpack $ serialize s
  -- fold over that list of bytes, encrypting as you go
  -- the state inherent in the fold is the "shift register"
  let (iv',encDat) = foldl magic (iv,BS.empty) dat
  -- Log what we are sending (encrypted and not)
  lift . putStrLn $ "Sending Encrypted: "
  lift . putStrLn $ indentedHex (serialize s)
  lift . putStrLn $ indentedHex encDat
  -- actually send it
  rPut encDat
  -- return the updated shift register to be used next time
  return iv'
  where
    -- Does a single step (one byte) of a CFB8 encryption
    magic (s,ds) d = let 
      -- encrypt the shift register
      s' = ecbEncrypt ciph s
      -- use the MSB of the shift register to encrypt the current plaintext
      ct = BS.head s' `xor` d
      -- shift the new ciphertext into the shift register
      s'' = BS.tail s `BS.snoc` ct
      -- add the cipher text to the output, and return the updated shift register
      in (s'',ds `BS.snoc` ct)

-- Get an unparsed packet from the network
getRawPacket :: (HasIO r, Member (Reader Handle) r) => Eff r BS.ByteString
getRawPacket = do
  -- Get the length it should be
  (l,bs) <- getPacketLength 
  -- Get (length) bytes more for the rest of the packet
  pktData <- rGet (fromIntegral l)
  -- TODO: make this a debugmode only putStrLn
  lift $ putStrLn "Got:"
  lift . putStrLn $ indentedHex $ bs `BS.append` pktData
  -- Return the actual data (no length annotation, but yes pktId)
  return pktData

-- TODO: merge with parseVarInt by abstracting the first line
getPacketLength :: (HasIO r, Member (Reader Handle) r) => Eff r (VarInt,BS.ByteString)
getPacketLength = do
  -- Get the first byte
  l <- BS.head <$> rGet 1
  -- the value part is the 7 least significant bits
  let thisPart = (unsafeCoerce :: Word8 -> VarInt) (clearBit l 7)
  -- If the msb is set, we have more bytes after this one
  if testBit l 7
    then do
      -- get the rest of the bytes recursively
      (next,bs) <- getPacketLength 
      -- return after shifting everything into place
      return $ (thisPart .|. (shiftL next 7),l `BS.cons` bs)
    -- If its not set, then this is the last byte, so we return thisPart
    else return (thisPart,BS.singleton l)

-- Get a packet from the network (high level) using a parser context
getPacket :: (HasIO r,Member (Reader Handle) r) => ServerState -> Eff r (Maybe Server.Packet)
getPacket st = do
  -- get the raw data (sans length)
  pkt <- getRawPacket
  -- parse it
  case parse (parsePacket st) "" pkt of
    -- If it parsed ok, then 
    Right serverPkt -> do
      -- return it
      return $ Just serverPkt
    -- If it didn't parse correctly, print the error and return that it parsed bad
    Left e -> lift (print e) >> return Nothing

-- add "  " to each line
shittyIndent :: String -> String
shittyIndent = init . unlines . map ("  "++) . lines

-- indent the hexdump
indentedHex :: BS.ByteString -> String
indentedHex = shittyIndent . prettyHex

-- get the auth info from the mojang server
authGetReq :: HasIO r => String -> String -> Eff r (Maybe (String,String))
authGetReq name hash = do
  -- use SSL
  manager <- lift $ newManager tlsManagerSettings
  -- create the request, using sId and username from LoginStart
  req <- lift $ parseRequest $ "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  -- actually make the request and store it
  resp <- lift $ httpLbs req manager
  -- parse the response into something useful
  case parse parseAuthJSON "" (LBS.toStrict . responseBody $ resp) of
    Left e -> do
      lift $ putStrLn "Failed to parse Auth JSON"
      lift $ print e
      return Nothing
    Right (uuid,plaName) -> return $ Just (uuid,plaName)

-- Parse an auth string, just getting the uuid and username
parseAuthJSON :: Parser (String,String)
parseAuthJSON = do
  _ <- C.string "{\"id\":\""
  uuid <- many C.hexDigit
  _ <- C.string "\",\"name\":\""
  plaName <- many (C.noneOf "\"")
  _ <- many anyChar
  eof
  return (reformat uuid,plaName)
  where
    -- add a hyphen at an index
    ins i s = let (a,b) = splitAt i s in a ++ "-" ++ b
    -- reformat the uuid to what the client expects
    reformat = ins 8 . ins 12 . ins 16 . ins 20

