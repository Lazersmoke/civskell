{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Civskell.Tech.Network
  (module Civskell.Data.Networking
  ,parseFromSet,getGenericPacket,serverAuthentication, clientAuthentication--,getPacketFromParser --,sendPacket,getPacket
  ) where

import Control.Eff (Eff,send)
import Data.Bits
import Data.Foldable
import Data.Bytes.Get
import Data.Bytes.Serial
import Data.Semigroup ((<>))
import Data.Word (Word8)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Attoparsec.ByteString
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.SuchThat
import qualified Data.Text as T

import Civskell.Data.Logging
import Civskell.Data.Networking
import Civskell.Data.Types

parseFromSet :: MonadGet m => ParseSet -> m (SuchThat '[Serial] DescribedPacket)
parseFromSet s = deserialize @VarInt >>= \pktId -> case find (\x -> ambiguously packetId x == pktId) s of
  Just (SuchThat (desc :: PacketDescriptor p)) -> do
    theP <- deserialize @p
    return $ ambiguate $ DescribedPacket desc theP
  Nothing -> error $ "No parser for that packet (Id = " <> show pktId <> ")"

-- Takes an effect to decide which parser to use once the packet arrives, and returns the parsed packet when it arrives
getGenericPacket :: forall r. (Logs r,Networks r) => Eff r ParseSet -> Eff r (Maybe (SuchThat '[Serial] DescribedPacket))
getGenericPacket ep = do
  -- Get the raw data (sans length)
  pkt <- removeCompression =<< getRawPacket
  -- TODO: Take advantage of incremental parsing maybe
  -- Parse it
  p <- ep
  case runGetS (parseFromSet p) pkt of
    -- If it parsed ok, then
    Right serverPkt@(SuchThat (DescribedPacket desc thePkt)) -> do
      -- Return it
      logLevel ServerboundPacket $ showPacket desc thePkt
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
  case parseOnly Aeson.json respJSON of
    Left e -> do
      loge $ "Failed to parse serverside Auth JSON because " <> T.pack (show e)
      loge $ "Malformed JSON was: " <> T.pack (show respJSON)
      loge $ "Full response was: " <> T.pack (show resp)
      return Nothing
    Right a -> case Aeson.fromJSON a of
      Aeson.Error e -> do
        loge $ "Parsed serverside Auth JSON, but it wasn't actual serverside Auth JSON because " <> T.pack e
        loge $ "Non-Auth JSON was: " <> T.pack (show a)
        loge $ "Full response was: " <> T.pack (show resp)
        return Nothing
      Aeson.Success authPacket -> return (Just authPacket)

clientAuthentication :: (Logs r,PerformsIO r) => (String,String) -> Eff r (Maybe ClientAuthResponse)
clientAuthentication (user,pass) = do
  -- Use SSL
  manager <- send $ newManager tlsManagerSettings
  let reqURL = "https://authserver.mojang.com/authenticate"
  logg $ "Sending authentication request to Mojang. Name: \"" <> T.pack user <> "\", Password: ********" -- Super secret
  logg $ "Request URL is \"" <> T.pack reqURL <> "\""
  let reqJSON = Aeson.object ["agent" .= Aeson.object ["name" .= ("Minecraft" :: T.Text), "version" .= (1 :: Integer)], "username" .= T.pack user, "password" .= T.pack pass, "requestUser" .= True]
  logg $ "Request JSON is " <> T.pack (show reqJSON)
  reqSansJSON <- send $ (parseRequest :: String -> IO Request) reqURL
  let req = reqSansJSON {method = "POST",requestBody = RequestBodyLBS $ Aeson.encode reqJSON}
  resp <- send $ httpLbs req manager
  let respJSON = LBS.toStrict . responseBody $ resp
  case parseOnly Aeson.json respJSON of
    Left e -> do
      loge $ "Failed to parse clientside Auth JSON because " <> T.pack (show e)
      loge $ "Malformed JSON was: " <> T.pack (show respJSON)
      loge $ "Full response was: " <> T.pack (show resp)
      return Nothing
    Right a -> case Aeson.fromJSON a of
      Aeson.Error e -> do
        loge $ "Parsed clientside Auth JSON, but it wasn't actual clientside Auth JSON because " <> T.pack e
        loge $ "Non-Auth JSON was: " <> T.pack (show a)
        loge $ "Full response was: " <> T.pack (show resp)
        return Nothing
      Aeson.Success cliAuthResp -> return (Just cliAuthResp)
