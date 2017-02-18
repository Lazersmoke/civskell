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
import Control.Eff
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
  manager <- liftIO $ newManager tlsManagerSettings
  -- create the request, using sId and username from LoginStart
  req <- liftIO $ parseRequest $ "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  -- actually make the request and store it
  resp <- liftIO $ httpLbs req manager
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

runNetworking :: HasIO r => Maybe EncryptionCouplet -> Maybe VarInt -> Handle -> Eff (Networking ': r) a -> Eff r a
runNetworking _ _ _ (Pure x) = Pure x
runNetworking mEnc mThresh hdl (Eff u q) = case u of 
  Weaken restOfU -> Eff restOfU (Singleton (runNetworking mEnc mThresh hdl . runTCQ q))
  Inject (GetFromNetwork len) -> do
    case mEnc of
      Nothing -> do
        dat <- liftIO (BS.hGet hdl len)
        runNetworking mEnc mThresh hdl (runTCQ q dat)
      Just (c,e,d) -> do
        bs <- liftIO (BS.hGet hdl len)
        let (bs',d') = cfb8Decrypt c d bs
        runNetworking (Just (c,e,d')) mThresh hdl (runTCQ q bs')
  Inject (PutIntoNetwork bs) -> do
    case mEnc of
      Nothing -> do
        liftIO (BS.hPut hdl bs)
        runNetworking mEnc mThresh hdl (runTCQ q ())
      Just (c,e,d) -> do
        let (bs',e') = cfb8Encrypt c e bs
        liftIO (BS.hPut hdl bs')
        runNetworking (Just (c,e',d)) mThresh hdl (runTCQ q ())
  Inject (SetCompressionLevel thresh) -> runNetworking mEnc thresh hdl (runTCQ q ())
  Inject (SetupEncryption couplet) -> runNetworking (Just couplet) mThresh hdl (runTCQ q ())
  Inject (AddCompression bs) -> case mThresh of
    -- Do not compress; annotate with length only
    Nothing -> runNetworking mEnc mThresh hdl (runTCQ q (withLength bs))
    Just t -> if BS.length bs >= fromIntegral t
      -- Compress data and annotate to match
      then do
        let compIdAndData = LBS.toStrict . Z.compress . LBS.fromStrict $ bs
        let origSize = serialize $ ((fromIntegral (BS.length bs)) :: VarInt)
        let ann = withLength (origSize <> compIdAndData)
        runNetworking mEnc mThresh hdl (runTCQ q ann)
      -- Do not compress; annotate with length only
      else do
        let ann = withLength (BS.singleton 0x00 <> bs)
        runNetworking mEnc mThresh hdl (runTCQ q ann)
  Inject (RemoveCompression bs) -> case mThresh of
    Nothing -> case parse parseUncompPkt "" bs of
      Left _ -> runNetworking mEnc mThresh hdl (runTCQ q bs)
      Right pktData -> runNetworking mEnc mThresh hdl (runTCQ q pktData)
    Just _ -> case parse parseCompPkt "" bs of
      -- TODO: fix this
      Left _ -> runNetworking mEnc mThresh hdl (runTCQ q bs)
      Right (dataLen,compressedData) -> if dataLen == 0x00
        then runNetworking mEnc mThresh hdl (runTCQ q compressedData)
        else runNetworking mEnc mThresh hdl (runTCQ q (LBS.toStrict . Z.decompress . LBS.fromStrict $ compressedData))

