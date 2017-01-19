module Lib where

import Control.Concurrent
import Network
import System.IO
import Data.Word
import Data.Bits
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.ByteString as BS

import Unsafe.Coerce

import CivNetwork
import Data
import Encrypt
import qualified Clientbound as Client
import qualified Serverbound as Server

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
  getPacket hdl Handshaking >>= maybe (putStrLn "WTF no packet? REEEEEEEEEEEEE") (handleHandshake hdl)

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
  mloginStart <- getPacket hdl LoggingIn
  case mloginStart of
    Just (Server.LoginStart name) -> do
      putStrLn $ "LoginStart from " ++ name
      (pub,priv) <- getAKeypair
      let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
      let sId = ""
      let encPubKey = encodePubKey pub
      sendSerial hdl (Client.EncryptionRequest sId encPubKey vt)
      mEncryptionResp <- getPacket hdl LoggingIn
      case mEncryptionResp of
        Just (Server.EncryptionResponse ssFromClient vtFromClient) -> do
          case RSA.decrypt Nothing priv vtFromClient of
            Left e -> print e
            Right vtHopefully -> do
              if vtHopefully /= vt
                then putStrLn "Client fucked up and encoded our vt wrong"
                else case RSA.decrypt Nothing priv ssFromClient of
                  Left e -> print e
                  Right ss -> do
                    let enc = makeEncrypter ss
                    let hash = genLoginHash sId ss encPubKey
                    print hash
                    auth <- authGetReq name hash
                    case auth of
                      Nothing -> putStrLn "parse error on auth"
                      Just (uuid,nameFromAuth) -> do
                        iv' <- sendSerialEnc (enc,ss) hdl (Client.LoginSuccess uuid nameFromAuth)
                        putStrLn "Yay it all worked (maybe)"
        Just _ -> putStrLn "Expected EncryptionResponse, got something else"
        Nothing -> putStrLn "unable to parse encryptionresponse packet"
    Just _ -> putStrLn "Unexpected non-login start packet"
    Nothing -> putStrLn "Unable to parse login start packet"

-- Server list refresh ping/status cycle
statusMode :: Handle -> IO ()
statusMode hdl = do
  -- Wait for them to ask our status
  mpkt <- getPacket hdl Status
  case mpkt of
    -- Make sure they are actually asking for our status
    Just Server.StatusRequest -> do
      -- TODO: dynamic response
      let resp = "{\"version\":{\"name\":\"1.10.2\",\"protocol\":210},\"players\":{\"max\":1000,\"online\": 0},\"description\":{\"text\":\"meow\"}}"
      -- Send resp to clients
      sendSerial hdl (Client.StatusResponse resp)
      -- Wait for them to start a ping
      mping <- getPacket hdl Status
      case mping of
        -- Make sure they are actually pinging us, then send a pong right away
        Just (Server.StatusPing l) -> sendSerial hdl (Client.StatusPong l)
        Just _ -> putStrLn "Client sent non-ping packet, disconnecting"
        Nothing -> putStrLn "WTF bruh you got this far and fucked up"
    Just _ -> putStrLn "Client gave non status-request packet, disconnecting"
    Nothing -> putStrLn "WTF m8? you tried to ask for our status, then gave us a shitty packet? Who the fuck do you think I am!?!?!?"

-- Simple way to inject a text message into a json chat string
jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"
