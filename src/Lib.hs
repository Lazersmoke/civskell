{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Control.Concurrent
import Network
import System.IO
import Data.Word
import Data.Bits
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.PubKey.RSA as RSA
import Crypto.Cipher.AES
import qualified Data.ByteString as BS
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State

import Data.Void
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
  _ <- forkIO (runM $ runLogger $ runNetworking handle connHandler)
  -- Wait for another connection
  connLoop sock

connHandler :: (r ~ (s ': s'),HasLogging r,HasIO r,HasNetworking r) => Eff r () 
connHandler = do
  -- Get a packet from the client (should be a handshake)
  mHandshake <- getPacket Handshaking 
  maybe (logg "WTF no packet? REEEEEEEEEEEEE") handleHandshake mHandshake

handleHandshake :: (r ~ (s ': s'),HasLogging r,HasIO r,HasNetworking r) => Server.Packet -> Eff r ()
-- Normal handshake recieved
handleHandshake (Server.Handshake protocol addr port newstate) = do
  -- Log to console that we have a handshaking connection
  logg $ 
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
  logg $ "Unexpected non-handshake packet with Id: " ++ show (packetId pkt)
  -- Tell client to fuck off because they gave us a bad packet
  sendSerial (Client.Disconnect (jsonyText "Bad Packet D:"))


checkVTandSS :: RSA.PrivateKey -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
checkVTandSS priv vtFromClient ssFromClient actualVT = do
  -- Try to decrypt their vt response
  case RSA.decrypt Nothing priv vtFromClient of
    -- if it fails, print the error
    Left e -> Left "Failed to parse Verify Token"
    Right vtHopefully -> do
      -- if it succeeds, check that it is actually the original vt
      if vtHopefully /= actualVT
        -- if it isn't, throw a fit and disconnect the client
        then Left "Invalid Verify Token"
        -- If the verify token was ok, then decrypt the ss
        else case RSA.decrypt Nothing priv ssFromClient of
          -- If it fails, print the error
          Left e -> Left "Failed to parse Shared Secret"
          Right ss -> Right ss

encryptionPhase :: (r ~ (s ': s'),HasIO r,HasLogging r,HasNetworking r) => String -> Eff r ()
encryptionPhase nameFromLoginStart = do
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
  getPacket LoggingIn >>= \case
    Just (Server.EncryptionResponse ssFromClient vtFromClient) -> do
      case checkVTandSS priv vtFromClient ssFromClient vt of
        Left s -> sendSerial (Client.Disconnect s) >> logg s
        -- Hand off to authentication, also upgrade to using encryption
        Right ss -> do
          let rs = runState :: (HasNetworking r,HasIO r) => Eff (State (AES128,BS.ByteString) ': r) () -> (AES128,BS.ByteString) -> Eff r ((),(AES128,BS.ByteString))
          let stateComp = (authPhase encPubKey sId nameFromLoginStart)
          _ <- (rs stateComp (makeEncrypter ss,ss)) 
          return ()
    Just _ -> logg "Expected EncryptionResponse, got something else"
    Nothing -> logg "unable to parse encryptionresponse packet"

authPhase :: (HasIO r,HasLogging r,HasEncryption r,HasNetworking r) => BS.ByteString -> String -> String -> Eff r ()
authPhase pubKey sId name = do
  (_,ss) <- get :: HasEncryption r => Eff r (AES128,BS.ByteString)
  -- make the serverId hash for auth
  let hash = genLoginHash sId ss pubKey
  -- do the Auth stuff with Mojang
  authGetReq name hash >>= \case
    -- If the auth is borked, its probably Mojangs fault tbh
    Nothing -> do
      logg "parse error on auth"
      -- Disclaim guilt
      sendSerialEnc (Client.Disconnect "Auth failed (not Lazersmoke's fault, probably!)")
      return ()
    Just (uuid,nameFromAuth) -> do
      -- Send a login success. We are now in play mode
      iv' <- sendSerialEnc (Client.LoginSuccess uuid nameFromAuth)
      logg "Yay it all worked (maybe)"
 
-- Do the login process
initiateLogin :: (r ~ (s ': s'),HasIO r,HasLogging r,HasNetworking r) => Eff r ()
initiateLogin = do
  -- Get a login start packet from the client
  mloginStart <- getPacket LoggingIn
  case mloginStart of
    -- LoginStart packets contain their username as a String
    Just (Server.LoginStart name) -> do
      -- Log that they connected
      logg $ "LoginStart from " ++ name
      encryptionPhase name
    Just _ -> logg "Unexpected non-login start packet"
    Nothing -> logg "Unable to parse login start packet"

-- Server list refresh ping/status cycle
statusMode :: (HasLogging r,HasNetworking r) => Eff r ()
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
        Just _ -> logg "Client sent non-ping packet, disconnecting"
        Nothing -> logg "WTF bruh you got this far and fucked up"
    Just _ -> logg "Client gave non status-request packet, disconnecting"
    Nothing -> logg "WTF m8? you tried to ask for our status, then gave us a shitty packet? Who the fuck do you think I am!?!?!?"

startPlaying :: (HasIO r,HasNetworking r,HasEncryption r) => Eff r ()
startPlaying = return ()

-- Simple way to inject a text message into a json chat string
jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"
