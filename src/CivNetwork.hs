{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import qualified Codec.Compression.Zlib as Z
import Data.Semigroup

import Data
import ParseBS
import qualified Serverbound as Server
import qualified Clientbound as Client
import Encrypt

-- Send something serializeable over the network
sendPacket :: (HasLogging r,HasNetworking r) => Client.Packet -> Eff r ()
sendPacket s = do
  -- Log its hex dump
  logg $ "Sending: "
  logg $ show s
  logg $ indentedHex (serialize s)
  -- Send it
  s' <- send . AddCompression $ serialize s
  rPut s'

-- Send something serializeable, but encrypt it using the given cipher and iv
sendPacketEnc :: (HasLogging r, HasNetworking r,HasEncryption r) => Client.Packet -> Eff r ()
sendPacketEnc s = do
  -- Only the data is compressed
  dat' <- send . AddCompression $ serialize s
  -- The entire connection is encrypted, not just the data
  dat <- cfb8Encrypt dat'
  -- Log what we are sending (encrypted and not)
  logg $ "Sending Encrypted: "
  logg $ show s
  logg $ indentedHex (serialize s)
  logg $ indentedHex dat'
  logg $ indentedHex dat
  -- actually send it
  rPut dat

-- Get an unparsed packet from the network
-- Returns the full, length annotated packet
getRawPacket :: (HasNetworking r) => Eff r BS.ByteString
getRawPacket = do
  -- Get the length it should be
  (l,_) <- getPacketLength 
  -- Get (length) bytes more for the rest of the packet
  pktData <- rGet (fromIntegral l)
  -- Return the actual data (no length annotation, but yes pktId)
  return pktData

-- TODO: merge with parseVarInt by abstracting the first line
getPacketLength :: (HasNetworking r) => Eff r (VarInt,BS.ByteString)
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

-- TODO: merge with parseVarInt by abstracting the first line
getPacketLengthEnc :: (HasEncryption r, HasNetworking r) => Eff r (VarInt,BS.ByteString)
getPacketLengthEnc = do
  -- Get the first byte
  l <- BS.head <$> rGetEnc 1
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


-- TODO: Completely redo this shit
getPacketEnc :: (HasEncryption r, HasLogging r, HasNetworking r) => ServerState -> Eff r (Maybe Server.Packet)
getPacketEnc st = do
  pkt' <- getRawPacketEnc
  pkt <- send . RemoveCompression $ pkt'
  -- parse it
  case parse (parsePacket st) "" pkt of
    -- If it parsed ok, then 
    Right serverPkt -> do
      -- TODO: make this a debugmode only putStrLn
      logg "Got:"
      logg $ show serverPkt
      logg $ indentedHex $ pkt'
      -- return it
      return $ Just serverPkt
    -- If it didn't parse correctly, print the error and return that it parsed bad
    Left e -> logg (show e) >> return Nothing

-- Get an unparsed packet from the network
-- Returns the full, length annotated packet
getRawPacketEnc :: (HasEncryption r, HasNetworking r) => Eff r BS.ByteString
getRawPacketEnc = do
  -- Get the length it should be
  (l,_) <- getPacketLengthEnc
  -- Get (length) bytes more for the rest of the packet
  pktData <- rGetEnc (fromIntegral l)
  -- Return the actual data (no length annotation, but yes pktId)
  return pktData

rGetEnc :: (HasEncryption r, HasNetworking r) => Int -> Eff r BS.ByteString
rGetEnc i = do
  bs <- rGet i
  cfb8Decrypt bs

-- Get a packet from the network (high level) using a parser context
getPacket :: (HasLogging r,HasNetworking r) => ServerState -> Eff r (Maybe Server.Packet)
getPacket st = do
  -- get the raw data (sans length)
  pkt' <- getRawPacket
  pkt <- send . RemoveCompression $ pkt'
  -- parse it
  case parse (parsePacket st) "" pkt of
    -- If it parsed ok, then 
    Right serverPkt -> do
      -- return it
      -- TODO: make this a debugmode only putStrLn
      logg "Got:"
      logg $ show serverPkt
      logg $ indentedHex $ pkt'
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

runNetworking :: HasIO r => Maybe VarInt -> Handle -> Eff (Networking ': r) a -> Eff r a
runNetworking _ _ (Val x) = return x
runNetworking mThresh hdl (E u q) = case decomp u of 
  Right now -> match now 
  Left restOfU -> E restOfU (tsingleton (\x -> runNetworking mThresh hdl (qApp q x)))
  where
    match (GetFromNetwork len) = do
      dat <- liftIO (BS.hGet hdl len)
      runNetworking mThresh hdl (qApp q dat)
    match (PutIntoNetwork bs) = do
      liftIO (BS.hPut hdl bs)
      runNetworking mThresh hdl (qApp q ())
    match (SetCompressionLevel thresh) = runNetworking thresh hdl (qApp q ())
    match (AddCompression bs) = case mThresh of
      -- Do not compress; annotate with length only
      Nothing -> runNetworking mThresh hdl (qApp q (withLength bs))
      Just t -> if BS.length bs >= fromIntegral t
        -- Compress data and annotate to match
        then do
          let compIdAndData = LBS.toStrict . Z.compress . LBS.fromStrict $ bs
          let origSize = serialize $ ((fromIntegral (BS.length bs)) :: VarInt)
          let ann = withLength (origSize <> compIdAndData)
          runNetworking mThresh hdl (qApp q ann)
        -- Do not compress; annotate with length only
        else do
          let ann = withLength (BS.singleton 0x00 <> bs)
          runNetworking mThresh hdl (qApp q ann)
    match (RemoveCompression bs) = case mThresh of
      Nothing -> runNetworking mThresh hdl (qApp q bs)
      Just _ -> case parse parseCompPkt "" bs of
        -- TODO: fix this
        Left _ -> runNetworking mThresh hdl (qApp q bs)
        Right (dataLen,compressedData) -> if dataLen == 0x00
          then runNetworking mThresh hdl (qApp q compressedData)
          else runNetworking mThresh hdl (qApp q (LBS.toStrict . Z.decompress . LBS.fromStrict $ compressedData))


