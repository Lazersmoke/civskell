{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provides high level networking capabilites.
module Civskell.Tech.Network 
  (getPacket 
  ,serverAuthentication
  ,clientAuthentication
  ) where

import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Serial
import Data.Semigroup ((<>))
import Data.Maybe
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
import qualified Data.Vector as Vector
import Control.Monad.Reader
import Control.Concurrent.STM

import Civskell.Data.Types
import Civskell.Data.Logging
import Civskell.Data.Networking

-- | Given a set of @'SupportedPackets'@, make a parser for @'DescribedPacket'@s.
parseFromSet :: MonadGet m => SupportedPackets PacketHandler -> m (ForAny (DescribedPacket PacketHandler))
parseFromSet s = deserialize @VarInt >>= \pktId -> case s Vector.!? (fromIntegral pktId) of -- Parse a VarInt and look it up in the packet vector
  -- If the item is supported, deserialize the rest of the data and return it
  Just (SuchThat (desc :: PacketDescriptor PacketHandler p)) -> pure . ambiguate . DescribedPacket desc =<< deserialize @p
  -- If the item is not supported, error log about it
  Nothing -> error $ "No parser for that packet (Id = " <> show pktId <> ")"

-- | Given a packet set selector, parse a packet from the network and return it if successful.
-- Note that this will hang until the packet arrives, and run the selector once it has arrived.
-- It needs to be like this because the @'ServerState'@ might change during the hang due to
-- parallel packet processing.
getPacket
  :: Civskell (SupportedPackets PacketHandler) -- ^ An effect to run that will select the appropriate @'SupportedPackets'@ set to use
  -> Civskell (Maybe (ForAny (DescribedPacket PacketHandler)))
getPacket ep = getRawPacket >>= \rawPkt -> decompressPacket rawPkt >>= \case
    Left e -> do
      compd <- fmap isJust . lift . readTVarIO =<< asks networkCompressionThreshold
      loge $ "Failed to decode incoming " <> (if compd then "compressed" else "uncompressed") <> " packet"
      loge . T.pack $ show e
      loge . T.pack . indentedHex $ rawPkt
      return Nothing
    -- The raw data (sans length)
    Right pkt -> do
      -- TODO: Take advantage of incremental parsing maybe
      -- Parse it
      p <- ep
      case runGetS (parseFromSet p) pkt of
        -- If it parsed ok, then
        Right serverPkt@(SuchThat (DescribedPacket desc thePkt)) -> do
          -- Return it
          logLevel VerboseLog "Parsed serverbound packet"
          logLevel ServerboundPacket $ showPacket desc thePkt
          logLevel HexDump . T.pack . indentedHex $ pkt
          return $ Just serverPkt
        -- If it didn't parse correctly, print the error and return Nothing
        Left e -> do
          loge "Failed to parse incoming packet"
          loge . T.pack $ show e
          loge . T.pack . indentedHex $ pkt
          return Nothing

-- | Get an unparsed packet from the network.
-- Returns the full, length annotated packet.
getRawPacket :: Civskell BS.ByteString
getRawPacket = do
  -- Get the length it should be
  (l,lbs) <- getPacketLength
  -- Get (length) bytes more for the rest of the packet
  pktData <- rGet (fromIntegral l)
  -- Return the actual data (no length annotation, but yes pktId)
  return (lbs <> pktData)

-- TODO: merge with parseVarInt by abstracting the first line
-- | Parses a @'VarInt'@ off the packet to determine the length.
-- The output is a pair of that @'VarInt'@ and the bytes representing it,
-- which are needed to re-assemble the full packet at a later time.
getPacketLength :: Civskell (VarInt,BS.ByteString)
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

-- | Given the needed information, authenticate a player with Mojang.
-- This performs a request to the @/hasJoined@ API endpoint.
serverAuthentication
  :: String -- ^ Player username
  -> String -- ^ Server hash
  -> Civskell (Maybe AuthPacket) -- ^ @'Just'@ the auth information, or @'Nothing'@ if something went wrong
serverAuthentication name hash = do
  -- Use SSL
  manager <- lift $ newManager tlsManagerSettings
  let reqURL = "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  logg $ "Sending authentication request to Mojang. Name: \"" <> T.pack name <> "\", Hash: \"" <> T.pack hash <> "\"."
  logg $ "Request URL is \"" <> T.pack reqURL <> "\""
  -- Create the request, using sId and username from LoginStart
  req <- lift $ (parseRequest :: String -> IO Request) reqURL 
  -- Actually make the request and store it
  resp <- lift $ httpLbs req manager
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

-- | Performs auth from the client side.
-- Not used in Civskell right now.
clientAuthentication :: (String,String) -> Civskell (Maybe ClientAuthResponse)
clientAuthentication (user,pass) = do
  -- Use SSL
  manager <- lift $ newManager tlsManagerSettings
  let reqURL = "https://authserver.mojang.com/authenticate"
  logg $ "Sending authentication request to Mojang. Name: \"" <> T.pack user <> "\", Password: ********" -- Super secret
  logg $ "Request URL is \"" <> T.pack reqURL <> "\""
  let reqJSON = Aeson.object ["agent" .= Aeson.object ["name" .= ("Minecraft" :: T.Text), "version" .= (1 :: Integer)], "username" .= T.pack user, "password" .= T.pack pass, "requestUser" .= True]
  logg $ "Request JSON is " <> T.pack (show reqJSON)
  reqSansJSON <- lift $ (parseRequest :: String -> IO Request) reqURL
  let req = reqSansJSON {method = "POST",requestBody = RequestBodyLBS $ Aeson.encode reqJSON}
  resp <- lift $ httpLbs req manager
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
