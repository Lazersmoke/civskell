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
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State

import Data
import ParseBS
import qualified Serverbound as Server
import Encrypt

-- Send something serializeable over the network
sendSerial :: (HasLogging r,HasNetworking r,Serialize s) => s -> Eff r ()
sendSerial s = do
  -- Log its hex dump
  logg $ "Sending: "
  logg $ indentedHex (serialize s)
  -- Send it
  rPut $ serialize s

-- Send something serializeable, but encrypt it using the given cipher and iv
sendSerialEnc :: (HasLogging r, HasNetworking r,HasEncryption r,Serialize s) => s -> Eff r ()
sendSerialEnc s = do
  -- Encrypt the data
  dat <- cfb8Encrypt (serialize s)
  -- Log what we are sending (encrypted and not)
  logg $ "Sending Encrypted: "
  logg $ indentedHex (serialize s)
  logg $ indentedHex dat
  -- actually send it
  rPut dat

-- Get an unparsed packet from the network
getRawPacket :: (HasLogging r, HasNetworking r) => Eff r BS.ByteString
getRawPacket = do
  -- Get the length it should be
  (l,bs) <- getPacketLength 
  -- Get (length) bytes more for the rest of the packet
  pktData <- rGet (fromIntegral l)
  -- TODO: make this a debugmode only putStrLn
  logg "Got:"
  logg $ indentedHex $ bs `BS.append` pktData
  -- Return the actual data (no length annotation, but yes pktId)
  return pktData

-- TODO: merge with parseVarInt by abstracting the first line
getPacketLength :: (HasLogging r, HasNetworking r) => Eff r (VarInt,BS.ByteString)
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
getPacket :: (HasLogging r,HasNetworking r) => ServerState -> Eff r (Maybe Server.Packet)
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
    Left e -> logg (show e) >> return Nothing

-- add "  " to each line
shittyIndent :: String -> String
shittyIndent = init . unlines . map ("  "++) . lines

-- indent the hexdump
indentedHex :: BS.ByteString -> String
indentedHex = shittyIndent . prettyHex

-- get the auth info from the mojang server
authGetReq :: (HasLogging r,HasIO r) => String -> String -> Eff r (Maybe (String,String))
authGetReq name hash = do
  -- use SSL
  manager <- liftIO $ newManager tlsManagerSettings
  -- create the request, using sId and username from LoginStart
  req <- liftIO $ parseRequest $ "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  -- actually make the request and store it
  resp <- liftIO $ httpLbs req manager
  -- parse the response into something useful
  case parse parseAuthJSON "" (LBS.toStrict . responseBody $ resp) of
    Left e -> do
      logg "Failed to parse Auth JSON"
      logg $ show e
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

