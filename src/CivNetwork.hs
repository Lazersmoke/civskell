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

import Data
import ParseBS
import qualified Serverbound as Server

-- Send something serializeable over the network
sendSerial :: Serialize s => Handle -> s -> IO ()
sendSerial hdl s = do
  -- Log its hex dump
  putStrLn $ "Sending: "
  putStrLn $ indentedHex (serialize s)
  -- Send it
  BS.hPut hdl (serialize s)

sendSerialEnc :: Serialize s => (AES128,BS.ByteString) -> Handle -> s -> IO BS.ByteString
sendSerialEnc (ciph,iv) hdl s = do
  let dat = BS.unpack $ serialize s
  let (iv',encDat) = foldl magic (iv,BS.empty) dat
  putStrLn $ "Sending Encrypted: "
  putStrLn $ indentedHex (serialize s)
  putStrLn $ indentedHex encDat
  BS.hPut hdl encDat
  return iv'
  where
    shiftBS b c = BS.tail b `BS.snoc` c
    magic (s,ds) d = let 
      -- MSB of Encrypted S sub I Minus One
      s' = ecbEncrypt ciph s
      -- Cipher text
      ct = BS.head s' `xor` d
      s'' = shiftBS s ct
      in (s'',ds `BS.snoc` ct)

-- Get an unparsed packet from the network
getRawPacket :: Handle -> IO BS.ByteString
getRawPacket handle = do
  -- Get the length it should be
  (l,bs) <- getPacketLength handle
  -- Get (length) bytes more for the rest of the packet
  pktData <- BS.hGet handle (fromIntegral l)
  -- TODO: make this a debugmode only putStrLn
  putStrLn "Got:"
  putStrLn $ indentedHex $ bs `BS.append` pktData
  -- Return the actual data (no length annotation, but yes pktId)
  return pktData

-- TODO: merge with parseVarInt by abstracting the first line
getPacketLength :: Handle -> IO (VarInt,BS.ByteString)
getPacketLength hdl = do
  l <- BS.head <$> BS.hGet hdl 1
  let thisPart = (unsafeCoerce :: Word8 -> VarInt) (clearBit l 7)
  if testBit l 7
    then do
      (next,bs) <- getPacketLength hdl
      return $ (thisPart .|. (shiftL next 7),l `BS.cons` bs)
    else return (thisPart,BS.singleton l)

-- Get a packet from the network (high level) using a parser context
getPacket :: Handle -> ServerState -> IO (Maybe Server.Packet)
getPacket hdl st = do
  -- get the raw data (sans length)
  pkt <- getRawPacket hdl
  -- parse it
  case parse (parsePacket st) "" pkt of
    -- If it parsed ok, then 
    Right serverPkt -> do
      -- return it
      return $ Just serverPkt
    -- If it didn't parse correctly, print the error and return that it parsed bad
    Left e -> print e >> return Nothing

shittyIndent :: String -> String
shittyIndent = init . unlines . map ("  "++) . lines

indentedHex :: BS.ByteString -> String
indentedHex = shittyIndent . prettyHex

authGetReq :: String -> String -> IO (Maybe (String,String))
authGetReq name hash = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ "https://sessionserver.mojang.com/session/minecraft/hasJoined?username=" ++ name ++ "&serverId=" ++ hash
  print req
  resp <- httpLbs req manager
  print resp
  case parse parseAuthJSON "" (LBS.toStrict . responseBody $ resp) of
    Left e -> do
      putStrLn "Failed to parse Auth JSON"
      print e
      return Nothing
    Right (uuid,plaName) -> return $ Just (uuid,plaName)

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
    ins i s = let (a,b) = splitAt i s in a ++ "-" ++ b
    reformat = ins 8 . ins 12 . ins 16 . ins 20

