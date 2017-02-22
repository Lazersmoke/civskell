{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Civskell.Tech.Network 
  (module Civskell.Data.Networking
  ,sendPacket,getPacket,authGetReq
  ) where

import Control.Eff (Eff,send)
import Data.Bits
import Data.Semigroup ((<>))
import Data.Word (Word8)
import Hexdump (prettyHex)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.Parsec
import Text.Parsec.ByteString (Parser)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Parsec.Char as C

import Civskell.Data.Types
import Civskell.Tech.Parse
import Civskell.Data.Logging
import Civskell.Data.Networking
import qualified Civskell.Packet.Serverbound as Server
import qualified Civskell.Packet.Clientbound as Client

-- Send something serializeable over the network
sendPacket :: (HasLogging r,HasNetworking r) => Client.Packet -> Eff r ()
sendPacket s = do
  -- Log its hex dump
  logLevel ClientboundPacket $ show s
  logLevel HexDump $ indentedHex (serialize s)
  -- Send it
  rPut =<< addCompression (serialize s)

-- Get a packet from the network (high level) using a parser context
getPacket :: (HasLogging r,HasNetworking r) => ServerState -> Eff r (Maybe Server.Packet)
getPacket st = do
  -- get the raw data (sans length)
  pkt <- removeCompression =<< getRawPacket
  -- parse it
  case parse (parsePacket st) "" pkt of
    -- If it parsed ok, then
    Right serverPkt -> do
      -- return it
      -- TODO: make this a debugmode only putStrLn
      logLevel ServerboundPacket $ show serverPkt
      logLevel HexDump $ indentedHex $ pkt
      return $ Just serverPkt
    -- If it didn't parse correctly, print the error and return that it parsed bad
    Left e -> do
      logLevel ErrorLog "Failed to parse incoming packet"
      logLevel ErrorLog (show e)
      -- Hex dump is an error
      logLevel ErrorLog $ indentedHex $ pkt
      return Nothing

-- Get an unparsed packet from the network
-- Returns the full, length annotated packet
getRawPacket :: (HasNetworking r) => Eff r BS.ByteString
getRawPacket = do
  -- Get the length it should be
  (l,lbs) <- getPacketLength
  -- Get (length) bytes more for the rest of the packet
  pktData <- rGet (fromIntegral l)
  -- Return the actual data (no length annotation, but yes pktId)
  return (lbs <> pktData)

-- TODO: merge with parseVarInt by abstracting the first line
getPacketLength :: (HasNetworking r) => Eff r (VarInt,BS.ByteString)
getPacketLength = do
  -- Get the first byte
  l' <- rGet 1
  let l = if BS.null l' then error "No more data on socket" else BS.head l'
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
  manager <- send $ newManager tlsManagerSettings
  -- create the request, using sId and username from LoginStart
  req <- send $ (parseRequest :: String -> IO Request) $ "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  -- actually make the request and store it
  resp <- send $ httpLbs req manager
  -- parse the response into something useful
  case parse parseAuthJSON "" (LBS.toStrict . responseBody $ resp) of
    Left e -> do
      logLevel ErrorLog "Failed to parse Auth JSON"
      logLevel ErrorLog $ show e
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
