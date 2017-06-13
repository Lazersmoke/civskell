{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Civskell.Tech.Network
  (module Civskell.Data.Networking
  ,parseFromSet,getGenericPacket,serverAuthentication --,getPacketFromParser --,sendPacket,getPacket
  ) where

import Control.Eff (Eff,send)
import Data.Functor.Identity
import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Serial
import Data.Semigroup ((<>))
import qualified Data.Map as Map
import Data.Word (Word8)
import Data.Aeson (fromJSON,json,Result(..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Attoparsec.ByteString
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.SuchThat
import qualified Data.Text as T
import qualified Data.Serialize.Get as Ser

import Civskell.Data.Logging
import Civskell.Data.Networking
import Civskell.Data.Types

parseFromSet :: MonadGet m => ParseSet m VarInt vc -> m (SuchThatStar vc)
parseFromSet s = deserialize @VarInt >>= \pktId -> case Map.lookup pktId s of
  Just cont -> cont
  Nothing -> error $ "No parser for that packet (Id = " <> show pktId <> ")"

-- Takes an effect to decide which parser to use once the packet arrives, and returns the parsed packet when it arrives
getGenericPacket :: forall c r. (Logs r,Networks r) => Eff r (ParseSet Ser.Get VarInt '[Packet,c]) -> Eff r (Maybe (SuchThatStar '[Packet,c]))
getGenericPacket ep = do
  -- Get the raw data (sans length)
  pkt <- removeCompression =<< getRawPacket
  -- TODO: Take advantage of incremental parsing maybe
  -- Parse it
  p <- ep
  case runGetS (parseFromSet p) pkt of
    -- If it parsed ok, then
    Right serverPkt -> do
      -- Return it
      logLevel ServerboundPacket . T.pack $ ambiguously (showPacket . runIdentity) serverPkt
      logLevel HexDump . T.pack . indentedHex $ pkt
      return $ Just serverPkt
    -- If it didn't parse correctly, print the error and return Nothing
    Left e -> do
      logLevel ErrorLog "Failed to parse incoming packet"
      logLevel ErrorLog . T.pack $ show e
      -- Hex dump is an error
      logLevel ErrorLog . T.pack . indentedHex $ pkt
      return Nothing

-- Get an unparsed packet from the network
-- Returns the full, length annotated packet
getRawPacket :: (Networks r) => Eff r BS.ByteString
getRawPacket = do
  -- Get the length it should be
  (l,lbs) <- getPacketLength
  -- Get (length) bytes more for the rest of the packet
  pktData <- rGet (fromIntegral l)
  -- Return the actual data (no length annotation, but yes pktId)
  return (lbs <> pktData)

-- TODO: merge with parseVarInt by abstracting the first line
getPacketLength :: (Networks r) => Eff r (VarInt,BS.ByteString)
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

-- Get the auth info from the mojang server
serverAuthentication :: (Logs r,PerformsIO r) => String -> String -> Eff r (Maybe AuthPacket)
serverAuthentication name hash = do
  -- Use SSL
  manager <- send $ newManager tlsManagerSettings
  let reqURL = "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  logg $ "Sending authentication request to Mojang. Name: \"" <> T.pack name <> "\", Hash: \"" <> T.pack hash <> "\"."
  logg $ "Request URL is \"" <> T.pack reqURL <> "\""
  -- Create the request, using sId and username from LoginStart
  req <- send $ (parseRequest :: String -> IO Request) reqURL 
  -- Actually make the request and store it
  resp <- send $ httpLbs req manager
  let respJSON = LBS.toStrict . responseBody $ resp
  -- Parse the response into something useful
  case parseOnly json respJSON of
    Left e -> do
      loge $ "Failed to parse Auth JSON because " <> T.pack (show e)
      loge $ "Malformed JSON was: " <> T.pack (show respJSON)
      loge $ "Full response was: " <> T.pack (show resp)
      return Nothing
    Right a -> case fromJSON a of
      Error e -> do
        loge $ "Parsed Auth JSON, but it wasn't actual Auth JSON because " <> T.pack e
        loge $ "Non-Auth JSON was: " <> T.pack (show a)
        loge $ "Full response was: " <> T.pack (show resp)
        return Nothing
      Success authPacket -> return (Just authPacket)

--clientAuthentication :: (Logs r,PerformsIO r) => 
