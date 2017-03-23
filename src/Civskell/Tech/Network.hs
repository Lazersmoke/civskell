{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Civskell.Tech.Network
  (module Civskell.Data.Networking
  ,sendClientPacket,getGenericPacket,authGetReq --,getPacketFromParser --,sendPacket,getPacket
  ) where

import Control.Eff (Eff,send)
import Data.Functor.Identity
import Data.Bits
import Data.Semigroup ((<>))
import Data.Word (Word8)
import Data.Aeson (fromJSON,json,Result(..))
import Hexdump (prettyHex)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Attoparsec.ByteString
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
--import qualified Text.Parsec.Char as C
import Data.SuchThat

import Civskell.Data.Logging
import Civskell.Data.Networking
import Civskell.Data.Types
--import Civskell.Tech.Parse

--import qualified Civskell.Packet.Clientbound as Client
--import qualified Civskell.Packet.Serverbound as Server

-- Send something serializeable over the network
--sendPacket :: (HasLogging r,HasNetworking r,Serialize p,Packet p,PacketSide p ~ 'Client) => p -> Eff r ()
--sendPacket = sendClientPacket . ClientPacket . ambiguate . Identity

sendClientPacket :: (HasLogging r,HasNetworking r) => ClientPacket s -> Eff r ()
sendClientPacket (ClientPacket (SuchThat (Identity (s :: a)))) = do
  -- Log its hex dump
  logLevel ClientboundPacket $ showPacket s
  logLevel HexDump $ indentedHex (serialize s)
  -- Send it
  rPut =<< addCompression (BS.append (serialize $ packetId @a) $ serialize s)

--getPacket :: forall p r. (HasLogging r,HasNetworking r,Packet p) => Eff r (Maybe p)
--getPacket = getPacketFromParser (parsePacket @p)
{-
-- Get a packet from the network (high level) using a parser context to decide which parser set to use
getPacketFromParser :: (HasLogging r,HasNetworking r,Packet p) => Parser p -> Eff r (Maybe p)
getPacketFromParser p = do
  -- Get the raw data (sans length)
  pkt <- removeCompression =<< getRawPacket
  -- TODO: Take advantage of incremental parsing maybe
  -- Parse it
  case parseOnly p pkt of
    -- If it parsed ok, then
    Right serverPkt -> do
      -- Return it
      logLevel ServerboundPacket $ showPacket serverPkt
      logLevel HexDump $ indentedHex $ pkt
      return $ Just serverPkt
    -- If it didn't parse correctly, print the error and return Nothing
    Left e -> do
      logLevel ErrorLog "Failed to parse incoming packet"
      logLevel ErrorLog (show e)
      -- Hex dump is an error
      logLevel ErrorLog $ indentedHex $ pkt
      return Nothing
-}
getGenericPacket :: (HasLogging r,HasNetworking r) => Parser (ForAny ServerPacket) -> Eff r (Maybe (ForAny ServerPacket))
getGenericPacket p = do
  -- Get the raw data (sans length)
  pkt <- removeCompression =<< getRawPacket
  -- TODO: Take advantage of incremental parsing maybe
  -- Parse it
  case parseOnly p pkt of
    -- If it parsed ok, then
    Right serverPkt -> do
      -- Return it
      logLevel ServerboundPacket $ ambiguously (\(ServerPacket x) -> ambiguously (showPacket . runIdentity) x) serverPkt
      logLevel HexDump $ indentedHex $ pkt
      return $ Just serverPkt
    -- If it didn't parse correctly, print the error and return Nothing
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
  -- The value part is the 7 least significant bits
  let thisPart = (unsafeCoerce :: Word8 -> VarInt) (clearBit l 7)
  -- If the msb is set, we have more bytes after this one
  if testBit l 7
    then do
      -- Get the rest of the bytes recursively
      (next,bs) <- getPacketLength
      -- Return after shifting everything into place
      return $ (thisPart .|. (shiftL next 7),l `BS.cons` bs)
    -- If its not set, then this is the last byte, so we return thisPart
    else return (thisPart,BS.singleton l)

-- Add "  " to each line
shittyIndent :: String -> String
shittyIndent = init . unlines . map ("  "++) . lines

-- Indent the hexdump
indentedHex :: BS.ByteString -> String
indentedHex = shittyIndent . prettyHex

-- Get the auth info from the mojang server
authGetReq :: (HasLogging r,HasIO r) => String -> String -> Eff r (Maybe AuthPacket)
authGetReq name hash = do
  -- Use SSL
  manager <- send $ newManager tlsManagerSettings
  -- Create the request, using sId and username from LoginStart
  req <- send $ (parseRequest :: String -> IO Request) $ "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  -- Actually make the request and store it
  resp <- send $ httpLbs req manager
  -- Parse the response into something useful
  case parseOnly parseAuthJSON (LBS.toStrict . responseBody $ resp) of
    Left e -> do
      logLevel ErrorLog "Failed to parse Auth JSON"
      logLevel ErrorLog $ show e
      return Nothing
    Right (Error e) -> do
      logLevel ErrorLog "Parsed bad JSON"
      logLevel ErrorLog $ show e
      return Nothing
    Right (Success a) -> return (Just a)

-- TODO: Use Aeson here
-- Parse an auth string, just getting the uuid and username
parseAuthJSON :: Parser (Data.Aeson.Result AuthPacket)
parseAuthJSON = fromJSON <$> json

