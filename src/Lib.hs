{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Control.Concurrent
import Network
import System.IO
import Data.Word
import Data.Bits
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.ByteString as BS
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict

import Unsafe.Coerce

import CivNetwork
import Data
import Encrypt
import qualified Clientbound as Client
import qualified Serverbound as Server

-- Send new connections to connLoop, and listen on 25565
-- Note for testing: Use virtualbox port forwarding to test locally
startListening :: IO ()
startListening = connLoop =<< listenOn (PortNumber 25565)

connLoop :: Socket -> IO ()
connLoop sock = do
  -- accept is what waits for a new connection
  (handle, cliHost, cliPort) <- accept sock
  -- Log that we got a connection
  putStrLn $ "Got Client connection: " ++ show cliHost ++ ":" ++ show cliPort
  -- Don't line buffer the network
  hSetBuffering handle NoBuffering
  -- We don't need the thread id
  _ <- forkIO (runLift $ runReader connHandler handle)
  -- Wait for another connection
  connLoop sock

connHandler :: (HasIO r,Member (Reader Handle) r) => Eff r () 
connHandler = do
  -- Get a packet from the client (should be a handshake)
  mHandshake <- getPacket Handshaking 
  maybe (lift $ putStrLn "WTF no packet? REEEEEEEEEEEEE") handleHandshake mHandshake

handleHandshake :: (HasIO r,Member (Reader Handle) r) => Server.Packet -> Eff r ()
-- Normal handshake recieved
handleHandshake (Server.Handshake protocol addr port newstate) = do
  -- Log to console that we have a handshaking connection
  lift . putStrLn $ 
    "Handshake: using protocol " ++ show protocol ++ 
    " and connecting to " ++ 
    addr ++ ":" ++ show port ++ 
    ", asking to go to " ++ show newstate
  case newstate of
    -- 1 for status
    1 -> statusMode 
    -- 2 for login
    2 -> initiateLogin 
    -- otherwise invalid TODO: This is invalid because it might have asked for status and fucked up, but this is only valid if it asks for login
    _ -> sendSerial (Client.Disconnect (jsonyText "Invalid Continuation State"))
-- Non-handshake (this is an error)
handleHandshake pkt = do
  -- Log to console that we have a bad packet
  lift . putStrLn $ "Unexpected non-handshake packet with Id: " ++ show (packetId pkt)
  -- Tell client to fuck off because they gave us a bad packet
  sendSerial (Client.Disconnect (jsonyText "Bad Packet D:"))

-- Do the login process
initiateLogin :: (HasIO r,Member (Reader Handle) r) => Eff r ()
initiateLogin = do
  -- Get a login start packet from the client
  mloginStart <- getPacket LoggingIn
  case mloginStart of
    -- LoginStart packets contain their username as a String
    Just (Server.LoginStart name) -> do
      -- Log that they connected
      lift $ putStrLn $ "LoginStart from " ++ name
      -- Get a new keypair to use with this client during the key exchange
      (pub,priv) <- getAKeypair
      -- Verify Token is fixed because why not
      let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
      -- sId is blank because (((history)))
      let sId = ""
      -- encode our public key into ASN.1's serialized format
      let encPubKey = encodePubKey pub
      -- Send an encryption request to the client
      sendSerial (Client.EncryptionRequest sId encPubKey vt)
      -- Wait for them to send an Encryption Response
      mEncryptionResp <- getPacket LoggingIn
      case mEncryptionResp of
        Just (Server.EncryptionResponse ssFromClient vtFromClient) -> do
          -- Try to decrypt their vt response
          case RSA.decrypt Nothing priv vtFromClient of
            -- if it fails, print the error
            Left e -> lift $ print e
            Right vtHopefully -> do
              -- if it succeeds, check that it is actually the original vt
              if vtHopefully /= vt
                -- if it isn't, throw a fit and disconnect the client
                then do
                  lift $ putStrLn "Client fucked up and encoded our vt wrong"
                  -- throw out new iv because we are done with them
                  sendSerial (Client.Disconnect "Invalid Verify Token")
                  return ()
                -- If the verify token was ok, then decrypt the ss
                else case RSA.decrypt Nothing priv ssFromClient of
                  -- If it fails, print the error
                  Left e -> lift $ print e
                  Right ss -> do
                    -- make the AES cipher context using the ss as the key
                    let enc = makeEncrypter ss
                    -- make the serverId hash for auth
                    let hash = genLoginHash sId ss encPubKey
                    -- do the Auth stuff with Mojang
                    auth <- authGetReq name hash
                    case auth of
                      -- If the auth is borked, its probably Mojangs fault tbh
                      Nothing -> do
                        lift $ putStrLn "parse error on auth"
                        -- Disclaim guilt
                        sendSerialEnc (enc,ss) (Client.Disconnect "Auth failed (not Lazersmoke's fault, probably!)")
                        return ()
                      Just (uuid,nameFromAuth) -> do
                        -- Send a login success. We are now in play mode
                        iv' <- sendSerialEnc (enc,ss) (Client.LoginSuccess uuid nameFromAuth)
                        lift $ putStrLn "Yay it all worked (maybe)"
        Just _ -> lift $ putStrLn "Expected EncryptionResponse, got something else"
        Nothing -> lift $ putStrLn "unable to parse encryptionresponse packet"
    Just _ -> lift $ putStrLn "Unexpected non-login start packet"
    Nothing -> lift $ putStrLn "Unable to parse login start packet"

-- Server list refresh ping/status cycle
statusMode :: (HasIO r,Member (Reader Handle) r) => Eff r ()
statusMode = do
  -- Wait for them to ask our status
  mpkt <- getPacket Status
  case mpkt of
    -- Make sure they are actually asking for our status
    Just Server.StatusRequest -> do
      -- TODO: dynamic response
      let resp = "{\"version\":{\"name\":\"1.10.2\",\"protocol\":210},\"players\":{\"max\":1000,\"online\": 0},\"description\":{\"text\":\"meow\"}}"
      -- Send resp to clients
      sendSerial (Client.StatusResponse resp)
      -- Wait for them to start a ping
      mping <- getPacket Status
      case mping of
        -- Make sure they are actually pinging us, then send a pong right away
        Just (Server.StatusPing l) -> sendSerial (Client.StatusPong l)
        Just _ -> lift $ putStrLn "Client sent non-ping packet, disconnecting"
        Nothing -> lift $ putStrLn "WTF bruh you got this far and fucked up"
    Just _ -> lift $ putStrLn "Client gave non status-request packet, disconnecting"
    Nothing -> lift $ putStrLn "WTF m8? you tried to ask for our status, then gave us a shitty packet? Who the fuck do you think I am!?!?!?"

-- Simple way to inject a text message into a json chat string
jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"
