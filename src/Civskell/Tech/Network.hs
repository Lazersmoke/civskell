{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Civskell.Tech.Network where

import Control.Concurrent.STM
import Control.Monad.Freer
import qualified Codec.Compression.Zlib as Z
import System.IO
import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Put
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
import qualified Data.Vector as Vector

import Civskell.Data.Types

import Civskell.Tech.Encrypt
import Civskell.Tech.Parse

parseFromSet :: MonadGet m => SupportedPackets PacketHandler -> m (ForAny (DescribedPacket PacketHandler))
parseFromSet s = deserialize @VarInt >>= \pktId -> case s Vector.!? (fromIntegral pktId) of
  Just (SuchThat (desc :: PacketDescriptor PacketHandler p)) -> do
    theP <- deserialize @p
    return $ ambiguate $ DescribedPacket desc theP
  Nothing -> error $ "No parser for that packet (Id = " <> show pktId <> ")"

-- Takes an effect to decide which parser to use once the packet arrives, and returns the parsed packet when it arrives
getGenericPacket :: forall r. Members '[Logging,Networking] r => Eff r (SupportedPackets PacketHandler) -> Eff r (Maybe (ForAny (DescribedPacket PacketHandler)))
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
getRawPacket :: Member Networking r => Eff r BS.ByteString
getRawPacket = do
  -- Get the length it should be
  (l,lbs) <- getPacketLength
  -- Get (length) bytes more for the rest of the packet
  pktData <- rGet (fromIntegral l)
  -- Return the actual data (no length annotation, but yes pktId)
  return (lbs <> pktData)

-- TODO: merge with parseVarInt by abstracting the first line
getPacketLength :: Member Networking r => Eff r (VarInt,BS.ByteString)
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
serverAuthentication :: Members '[Logging,IO] r => String -> String -> Eff r (Maybe AuthPacket)
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

clientAuthentication :: Members '[Logging,IO] r => (String,String) -> Eff r (Maybe ClientAuthResponse)
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


-- `Packeting`/`SendsPackets` is a high level interface that explicitly marks raw bytes as unsafe
runPacketing :: Members '[Logging,Networking] r => Eff (Packeting ': r) a -> Eff r a
runPacketing = generalizedRunNat handlePacketing

handlePacketing :: Members '[Logging,Networking] r => Packeting a -> Eff r a
handlePacketing = \case
  -- Unpack from the existentials to get the type information into a skolem, scoped tyvar
  SendPacket (SuchThat (DescribedPacket pktDesc pkt)) -> do
    let pktHandler = packetHandler pktDesc
    let writePacket = serializePacket pktHandler pkt
    -- Log its hex dump
    logLevel ClientboundPacket $ showPacket pktDesc pkt
    logLevel HexDump . T.pack $ indentedHex (runPutS writePacket)
    -- Send it
    rPut =<< addCompression (runPutS $ serialize (packetId pktHandler) *> writePacket)
  -- "Unsafe" means it doesn't come from a legitimate packet (doesn't have packetId) and doesn't get compressed
  UnsafeSendBytes bytes -> rPut bytes
  -- Passthrough for setting up encryption and compression
  BeginEncrypting ss -> setupEncryption ss
  BeginCompression thresh -> setCompression thresh

runNetworking :: Member IO r => TVar (Maybe EncryptionCouplet) -> TVar (Maybe VarInt) -> Handle -> Eff (Networking ': r) a -> Eff r a
runNetworking mEnc mThresh hdl = runNat (handleNetworking mEnc mThresh hdl)

handleNetworking :: TVar (Maybe EncryptionCouplet) -> TVar (Maybe VarInt) -> Handle -> Networking a -> IO a
handleNetworking mEnc mThresh hdl = \case
  --ForkNetwork e -> pure (runNetworking mEnc mThresh hdl e)
  GetFromNetwork len -> do
    -- We don't want exclusive access here because we will block
    dat <- BS.hGet hdl len
    -- Decrypt the data or don't based on the encryption value
    atomically $ readTVar mEnc >>= \case
      Nothing -> return dat
      Just (c,e,d) -> do
        -- d' is the updated decrypting shift register
        let (bs',d') = cfb8Decrypt c d dat
        writeTVar mEnc (Just (c,e,d'))
        return bs'
  PutIntoNetwork bs -> do
    -- Encrypt the data or don't based on the encryption value
    enc <- atomically $ readTVar mEnc >>= \case
      Nothing -> return bs
      Just (c,e,d) -> do
        -- e' is the updated encrypting shift register
        let (bs',e') = cfb8Encrypt c e bs
        writeTVar mEnc (Just (c,e',d))
        return bs'
    BS.hPut hdl enc
  SetCompressionLevel thresh -> atomically $ readTVar mThresh >>= \case
    -- If there is no pre-existing threshold, set one
    Nothing -> writeTVar mThresh (Just thresh)
    -- If there is already compression, don't change it (only one setCompression is allowed)
    -- Should we throw an error instead of silently failing here?
    Just _ -> pure ()
  SetupEncryption ss -> atomically $ readTVar mThresh >>= \case
    -- If there is no encryption setup yet, set it up
    Nothing -> writeTVar mEnc (Just (makeEncrypter ss,ss,ss))
    -- If it is already encrypted, don't do anything
    Just _ -> pure ()
  AddCompression bs -> readTVarIO mThresh >>= \case
    -- Do not compress; annotate with length only
    Nothing -> pure (runPutS . withLength $ bs)
    -- Compress with the threshold `t`
    Just t -> if BS.length bs >= fromIntegral t
      -- Compress data and annotate to match
      then do
        -- Compress the actual data
        let compIdAndData = putByteString . LBS.toStrict . Z.compress . LBS.fromStrict $ bs
        -- Add the original size annotation
        let origSize = serialize $ ((fromIntegral (BS.length bs)) :: VarInt)
        let ann = withLength (runPutS $ origSize >> compIdAndData)
        pure (runPutS ann)
      -- Do not compress; annotate with length only
      -- 0x00 indicates it is not compressed
      else pure . runPutS . withLength . runPutS $ putWord8 0x00 *> putByteString bs
  RemoveCompression bs -> readTVarIO mThresh >>= \case
    -- Parse an uncompressed packet
    Nothing -> case runGetS parseUncompPkt bs of
      -- TODO: fix this completely ignoring a parse error
      Left _ -> putStrLn "Emergency Parse Error!!!" *> pure bs
      Right pktData -> pure pktData
    -- Parse a compressed packet (compressed packets have extra metadata)
    Just _ -> case parseOnly parseCompPkt bs of
      -- TODO: fix this completely ignoring a parse error
      Left _ -> pure bs
      Right (dataLen,compressedData) -> pure $ if dataLen == 0
        -- dataLen of 0 means that the packet is actually uncompressed
        then compressedData
        -- If it is indeed compressed, uncompress it
        else LBS.toStrict . Z.decompress . LBS.fromStrict $ compressedData


