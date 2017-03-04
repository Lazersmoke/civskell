{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Civskell.Tech.Network
  (module Civskell.Data.Networking
  ,sendClientPacket,sendPacket,getPacket,getPacketFromParser,authGetReq
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
import qualified Data.Attoparsec.ByteString as Atto
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Parsec.Char as C
import Data.SuchThat

import Civskell.Data.Logging
import Civskell.Data.Networking
import Civskell.Data.Types
--import Civskell.Tech.Parse

--import qualified Civskell.Packet.Clientbound as Client
--import qualified Civskell.Packet.Serverbound as Server

-- Send something serializeable over the network
sendPacket :: (HasLogging r,HasNetworking r,Serialize p,Packet p,PacketSide p ~ 'Client) => p -> Eff r ()
sendPacket = sendClientPacket . ClientPacket . SuchThatStar

sendClientPacket :: (HasLogging r,HasNetworking r) => ClientPacket s -> Eff r ()
sendClientPacket (ClientPacket (SuchThatStar s)) = do
  -- Log its hex dump
  logLevel ClientboundPacket $ showPacket s
  logLevel HexDump $ indentedHex (serialize s)
  -- Send it
  rPut =<< addCompression (serialize s)

getPacket :: forall p r. (HasLogging r,HasNetworking r,Packet p) => Eff r (Maybe p)
getPacket = getPacketFromParser (parsePacket @p)

-- Get a packet from the network (high level) using a parser context to decide which parser set to use
getPacketFromParser :: (HasLogging r,HasNetworking r) => Atto.Parser p -> Eff r (Maybe p)
getPacketFromParser p = do
  -- Get the raw data (sans length)
  pkt <- removeCompression =<< getRawPacket
  -- TODO: Take advantage of incremental parsing maybe
  -- Parse it
  case Atto.parseOnly p pkt of
    -- If it parsed ok, then
    Right serverPkt -> do
      -- Return it
      --logLevel ServerboundPacket $ show serverPkt
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
authGetReq :: (HasLogging r,HasIO r) => String -> String -> Eff r (Maybe (String,String))
authGetReq name hash = do
  -- Use SSL
  manager <- send $ newManager tlsManagerSettings
  -- Create the request, using sId and username from LoginStart
  req <- send $ (parseRequest :: String -> IO Request) $ "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  -- Actually make the request and store it
  resp <- send $ httpLbs req manager
  -- Parse the response into something useful
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
    -- Add a hyphen at an index
    ins i s = let (a,b) = splitAt i s in a ++ "-" ++ b
    -- Reformat the uuid to what the client expects
    reformat = ins 8 . ins 12 . ins 16 . ins 20
