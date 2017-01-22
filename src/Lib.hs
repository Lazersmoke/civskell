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
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.PubKey.RSA as RSA
import qualified Data.ByteString as BS
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State

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
  _ <- forkIO (runM $ runLogger $ runNetworking Nothing handle connHandler)
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
    _ -> sendPacket (Client.Disconnect (jsonyText "Invalid Continuation State"))
-- Non-handshake (this is an error)
handleHandshake pkt = do
  -- Log to console that we have a bad packet
  logg $ "Unexpected non-handshake packet with Id: " ++ show (packetId pkt)
  -- Tell client to fuck off because they gave us a bad packet
  sendPacket (Client.Disconnect (jsonyText "Bad Packet D:"))


checkVTandSS :: RSA.PrivateKey -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
checkVTandSS priv vtFromClient ssFromClient actualVT = do
  -- Try to decrypt their vt response
  case RSA.decrypt Nothing priv vtFromClient of
    -- if it fails, print the error
    Left e -> Left $ "Failed to parse Verify Token: " ++ show e
    Right vtHopefully -> do
      -- if it succeeds, check that it is actually the original vt
      if vtHopefully /= actualVT
        -- if it isn't, throw a fit and disconnect the client
        then Left "Invalid Verify Token"
        -- If the verify token was ok, then decrypt the ss
        else case RSA.decrypt Nothing priv ssFromClient of
          -- If it fails, print the error
          Left e -> Left $ "Failed to parse Shared Secret: " ++ show e
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
  sendPacket (Client.EncryptionRequest sId encPubKey vt)
  -- Wait for them to send an Encryption Response
  getPacket LoggingIn >>= \case
    Just (Server.EncryptionResponse ssFromClient vtFromClient) -> do
      case checkVTandSS priv vtFromClient ssFromClient vt of
        Left s -> sendPacket (Client.Disconnect s) >> logg s
        -- Hand off to authentication, also upgrade to using encryption
        Right ss -> do
          let stateComp = authPhase encPubKey sId nameFromLoginStart
          _ <- (runState (runReader stateComp (makeEncrypter ss)) ss)
          return ()
    Just _ -> logg "Expected EncryptionResponse, got something else"
    Nothing -> logg "unable to parse encryptionresponse packet"

authPhase :: (HasIO r,HasLogging r,HasEncryption r,HasNetworking r) => BS.ByteString -> String -> String -> Eff r ()
authPhase pubKey sId name = do
  ss <- get :: HasEncryption r => Eff r BS.ByteString
  -- make the serverId hash for auth
  let hash = genLoginHash sId ss pubKey
  -- do the Auth stuff with Mojang
  authGetReq name hash >>= \case
    -- If the auth is borked, its probably Mojangs fault tbh
    Nothing -> do
      logg "parse error on auth"
      -- Disclaim guilt
      sendPacketEnc (Client.Disconnect "Auth failed (not Lazersmoke's fault, probably!)")
      return ()
    Just (uuid,nameFromAuth) -> do
      -- Tell the client to compress starting now. Agressive for testing
      -- EDIT: setting this to 3 gave us a bad frame exception :S
      --sendPacketEnc (Client.SetCompression 16)
      --setCompression $ Just 16
      -- Send a login success. We are now in play mode
      sendPacketEnc (Client.LoginSuccess uuid nameFromAuth)
      logg "Yay it all worked (maybe)"
      startPlaying

-- Start Play Mode, where we have just sent "Login Success"
startPlaying :: (HasLogging r, HasEncryption r, HasNetworking r) => Eff r ()
startPlaying = do
  -- EID, Gamemode (Enum), Dimension (Enum), Difficulty (Enum), Max Players (deprecated), Level Type, reduce debug info?
  sendPacketEnc (Client.JoinGame 0 0x01 0 0x00 0x00 "default" False)
  -- Tell them we aren't vanilla so no one gets mad at mojang for our fuck ups
  sendPacketEnc (Client.PluginMessage "MC|Brand" (serialize "civskell"))
  -- Difficulty to peacful
  sendPacketEnc (Client.ServerDifficulty 0x00)
  -- Home spawn position
  sendPacketEnc (Client.SpawnPosition (Position 0 64 0))
  -- Player Abilities
  sendPacketEnc (Client.PlayerAbilities 0x00 0.0 1.0)
  getPacketEnc Playing >>= \case
    Just (Server.PluginMessage "MC|Brand" cliBrand) -> do
      logg $ "Client brand is: " ++ show (BS.tail cliBrand)
    Just 
  
 
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
      sendPacket (Client.StatusResponse resp)
      -- Wait for them to start a ping
      mping <- getPacket Status
      case mping of
        -- Make sure they are actually pinging us, then send a pong right away
        Just (Server.StatusPing l) -> sendPacket (Client.StatusPong l)
        Just _ -> logg "Client sent non-ping packet, disconnecting"
        Nothing -> logg "WTF bruh you got this far and fucked up"
    Just _ -> logg "Client gave non status-request packet, disconnecting"
    Nothing -> logg "WTF m8? you tried to ask for our status, then gave us a shitty packet? Who the fuck do you think I am!?!?!?"

-- Simple way to inject a text message into a json chat string
jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"
