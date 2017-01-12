module Lib where

import Network
import qualified Data.ByteString as BS
import System.IO
import Control.Concurrent
import Text.Parsec
import Hexdump

import ParseBS
import qualified Serverbound as Server
import qualified Clientbound as Client
import Data
import Encrypt

startListening :: IO ()
startListening = connLoop =<< listenOn (PortNumber 25565)

connLoop :: Socket -> IO ()
connLoop sock = do
  -- accept is what waits for a new connection
  (handle, cliHost, cliPort) <- accept sock
  putStrLn $ "Got Client connection: " ++ show cliHost ++ ":" ++ show cliPort
  -- Don't line buffer the network
  hSetBuffering handle NoBuffering
  -- We don't need the thread id
  _ <- forkIO (connHandler handle)
  connLoop sock

connHandler :: Handle -> IO ()
connHandler hdl = do
  -- Get a packet from the client (should be a handshake)
  getPacket hdl >>= maybe (putStrLn "WTF no packet? REEEEEEEEEEEEE") (handleHandshake hdl)

handleHandshake :: Handle -> Server.Packet -> IO ()
-- Normal handshake recieved
handleHandshake hdl (Server.Handshake protocol addr port newstate) = do
  -- Log to console that we have a handshaking connection
  putStrLn $ 
    "Handshake: using protocol " ++ show protocol ++ 
    " and connecting to " ++ 
    addr ++ ":" ++ show port ++ 
    ", asking to go to " ++ show newstate
  case newstate of
    -- 1 for status
    1 -> statusMode hdl
    -- 2 for login
    2 -> initiateLogin hdl
    -- otherwise invalid TODO: This is invalid because it might have asked for status and fucked up, but this is only valid if it asks for login
    _ -> sendSerial hdl (Client.Disconnect (jsonyText "Invalid Continuation State"))
-- Non-handshake (this is an error)
handleHandshake hdl pkt = do
  -- Log to console that we have a bad packet
  putStrLn $ "Unexpected non-handshake packet with Id: " ++ show (packetId pkt)
  -- Tell client to fuck off because they gave us a bad packet
  sendSerial hdl (Client.Disconnect (jsonyText "Bad Packet D:"))

initiateLogin :: Handle -> IO ()
initiateLogin hdl = do
  mloginStart <- getPacket hdl
  case mloginStart of
    Just (Server.LoginStart name) -> do
      putStrLn $ "LoginStart from " ++ name
      (p,k) <- generatePublicKey
      let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
      sendSerial hdl (Client.EncryptionRequest "abc" p vt)
      mEncryptionResp <- getPacket hdl
      case mEncryptionResp of
        Just (Server.EncryptionResponse ss vt') -> do
          if decryptVT ss k vt' /= vt 
            then putStrLn "Client fucked up and encoded our vt wrong"
            else do
              let sharedSecret = decryptSS ss k
              putStrLn "Yay it all worked"
        Just a -> putStrLn "Expected EncryptionResponse, got something else"
statusMode :: Handle -> IO ()
statusMode hdl = do
  mpkt <- getPacket hdl
  case mpkt of
    Just Server.StatusRequest -> do
      let resp = "{\"version\":{\"name\":\"1.10.2\",\"protocol\":210},\"players\":{\"max\":1000,\"online\": 0},\"description\":{\"text\":\"meow\"}}"
      putStrLn resp
      sendSerial hdl (Client.StatusResponse resp)
      mping <- getPacket hdl
      case mping of
        Just (Server.StatusPing l) -> sendSerial hdl (Client.StatusPong l)
        Just a -> putStrLn "Client sent non-ping packet, disconnecting"
        Nothing -> putStrLn "WTF bruh you got this far and fucked up"
    Just a -> putStrLn "Client gave non status-request packet, disconnecting"
    Nothing -> putStrLn "WTF m8? you tried to ask for our status, then gave us a shitty packet? Who the fuck do you think I am!?!?!?"

jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"

sendSerial :: Serialize s => Handle -> s -> IO ()
sendSerial hdl s = do
  putStrLn $ "Sending: "
  putStrLn $ indentedHex (serialize s)
  BS.hPut hdl (serialize s)

getRawPacket :: Handle -> IO BS.ByteString
getRawPacket handle = do
  -- First byte has the length
  l <- BS.head <$> BS.hGet handle 1
  -- Get (length) bytes more for the rest of the packet
  pktData <- BS.hGet handle (fromEnum l)
  putStrLn "Got:"
  putStrLn . indentedHex $ l `BS.cons` pktData
  -- Return the actual data (no length annotation, but yes pktId)
  return pktData

getPacket :: Handle -> IO (Maybe Server.Packet)
getPacket hdl = do
  -- get the raw data (sans length)
  pkt <- getRawPacket hdl
  -- parse it
  case parse parsePacket "" pkt of
    -- If it parsed ok, then 
    Right serverPkt -> do
      -- Log about it
      putStrLn $ "Packet: " ++ show serverPkt
      -- return it
      return $ Just serverPkt
    -- If it didn't parse correctly, print the error and return that it parsed bad
    Left e -> print e >> return Nothing

shittyIndent :: String -> String
shittyIndent = init . unlines . map ("  "++) . lines

indentedHex :: BS.ByteString -> String
indentedHex = shittyIndent . prettyHex
