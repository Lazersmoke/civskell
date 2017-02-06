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
  _ <- forkIO (runM $ runLogger $ runNetworking Nothing Nothing handle connHandler)
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
  logg $ "Handshake: "
  logg $ "  Protocol: " ++ show protocol
  logg $ "  Server Address: " ++ addr ++ ":" ++ show port
  logg $ "  Next State: " ++ (case newstate of {1 -> "Status"; 2 -> "Login"; _ -> "Invalid"})
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
  logLevel ErrorLog $ "Unexpected non-handshake packet with Id: " ++ show (packetId pkt)
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
        Left s -> sendPacket (Client.Disconnect (jsonyText s)) >> logLevel ErrorLog s
        -- Hand off to authentication, also upgrade to using encryption
        Right ss -> do
          -- make the serverId hash for auth
          let loginHash = genLoginHash sId ss encPubKey
          setupEncryption (makeEncrypter ss, ss, ss)
          authPhase nameFromLoginStart loginHash
          return ()
    Just _ -> logg "Expected EncryptionResponse, got something else"
    Nothing -> logg "unable to parse encryptionresponse packet"

authPhase :: (r ~ (s ': s'),HasIO r,HasLogging r,HasNetworking r) => String -> String -> Eff r ()
authPhase name hash = do
  -- do the Auth stuff with Mojang
  authGetReq name hash >>= \case
    -- If the auth is borked, its probably Mojangs fault tbh
    Nothing -> do
      logLevel ErrorLog "Parse error on auth"
      -- Disclaim guilt
      sendPacket (Client.Disconnect $ jsonyText "Auth failed (not Lazersmoke's fault, probably!)")
      return ()
    Just (uuid,nameFromAuth) -> do
      -- Tell the client to compress starting now. Agressive for testing
      -- EDIT: setting this to 3 gave us a bad frame exception :S
      sendPacket (Client.SetCompression 16)
      setCompression $ Just 16
      -- Send a login success. We are now in play mode
      sendPacket (Client.LoginSuccess uuid nameFromAuth)
      logg "Yay it all worked! (maybe); starting play cycle"
      startPlaying

-- Start Play Mode, where we have just sent "Login Success"
startPlaying :: (r ~ (s ': s'),HasLogging r, HasNetworking r) => Eff r ()
startPlaying = do
  -- EID, Gamemode (Enum), Dimension (Enum), Difficulty (Enum), Max Players (deprecated), Level Type, reduce debug info?
  sendPacket (Client.JoinGame 0 0x01 0 0x00 0x00 "default" False)
  -- Tell them we aren't vanilla so no one gets mad at mojang for our fuck ups
  sendPacket (Client.PluginMessage "MC|Brand" (serialize "civskell"))
  -- Difficulty to peacful
  sendPacket (Client.ServerDifficulty 0x00)
  -- Home spawn position
  sendPacket (Client.SpawnPosition (Position 0 64 0))
  -- Player Abilities
  sendPacket (Client.PlayerAbilities 0x00 0.0 1.0)
  -- 0 is player inventory
  sendPacket (Client.WindowItems 0 (replicate 40 EmptySlot ++ [Slot 1 5 0 Nothing] ++ replicate 4 EmptySlot))
  initPlayer packetLoop

packetLoop :: (r ~ (s ': s'),HasPlayer r, HasLogging r, HasNetworking r) => Eff r ()
packetLoop = do
  mPkt <- getPacket Playing 
  maybe (logLevel ErrorLog "Failed to parse incoming packet") gotPacket mPkt 
  packetLoop

gotPacket :: (r ~ (s ': s'),HasPlayer r, HasLogging r, HasNetworking r) => Server.Packet -> Eff r ()
gotPacket (Server.PluginMessage "MC|Brand" cliBrand) = do
  logg $ "Client brand is: " ++ show (BS.tail cliBrand)
  setBrand $ show (BS.tail cliBrand)
gotPacket (Server.ClientSettings loc viewDist chatMode chatColors skin hand) = do
  logg $ "Client Settings:"
  logg $ "  Locale: " ++ loc 
  logg $ "  View Distance: " ++ show viewDist 
  logg $ "  Chat Mode: " ++ show chatMode 
  logg $ "  Chat colors enabled: " ++ show chatColors 
  logg $ "  Skin bitmask: " ++ show skin 
  logg $ "  Main Hand: " ++ show hand
  -- 0x00 means all absolute, 0x01 is tp confirm Id
  let tid = 0x01
  sendPacket (Client.PlayerPositionAndLook (Position 0.0 0.0 0.0) (0.0,0.0) 0x00 tid)
  pendTeleport tid
  logg $ "Teleport with id " ++ show tid ++ " pending"
gotPacket (Server.TPConfirm tid) = do
  c <- clearTeleport tid
  if c
    then logg $ "Client confirms teleport with id: " ++ show tid
    else logg $ "Client provided bad teleport id: " ++ show tid
gotPacket (Server.Animation hand) = do
  logg $ "Client is swinging " ++ (case hand of {0 -> "main hand"; 1 -> "off hand"})
  logg "Sending keep alive"
  sendPacket (Client.KeepAlive 5)
  -- Player EID is 0; mc encoding is retarded, don't ask me why off hand is 3
  --sendPacket (Client.Animation 0 (case hand of {0 -> 0; 1 -> 3}))
gotPacket (Server.HeldItemChange slotNum) = do
  logg $ "Player is holding slot number " ++ show slotNum
  setHolding slotNum
gotPacket (Server.ClientStatus status) = do
  logg $ "Client Status is: " ++ (case status of {0 -> "Perform Respawn"; 1 -> "Request Stats"; 2 -> "Open Inventory"})
gotPacket (Server.CreativeInventoryAction slotNum slotDat) = do
  logg $ "Player creatively set slot " ++ show slotNum ++ " to {" ++ show slotDat ++ "}"
gotPacket (Server.KeepAlive kid) = do
  logg $ "Player sent keep alive pong with id: " ++ show kid
gotPacket (Server.PlayerPosition (Position x y z) grounded) = do
  logg $ "Player is at " ++ show (x,y,z)
  logg $ if grounded then "Player is on the ground" else "Player is not on the ground"
gotPacket (Server.PlayerPositionAndLook (Position x y z) (yaw,pitch) grounded) = do
  logg $ "Player is at " ++ show (x,y,z) 
  logg $ "Player is looking to " ++ show (yaw,pitch)
  logg $ if grounded then "Player is on the ground" else "Player is not on the ground"
gotPacket (Server.PlayerLook (y,p) grounded) = do
  logg $ "Player is looking to " ++ show (y,p)
  logg $ if grounded then "Player is on the ground" else "Player is not on the ground"
gotPacket (Server.Player grounded) = do
  logg $ if grounded then "Player is on the ground" else "Player is not on the ground"
gotPacket (Server.CloseWindow wid) = do
  logg $ "Player is closing a window with id: " ++ show wid
gotPacket (Server.ChatMessage msg) = do
  logg $ "Player said: " ++ msg
gotPacket a = logLevel ErrorLog $ "Unsupported packet: " ++ show a
  
 
-- Do the login process
initiateLogin :: (r ~ (s ': s'),HasIO r,HasLogging r,HasNetworking r) => Eff r ()
initiateLogin = do
  -- Get a login start packet from the client
  mloginStart <- getPacket LoggingIn
  case mloginStart of
    -- LoginStart packets contain their username as a String
    Just (Server.LoginStart name) -> do
      -- Log that they connected
      logg $ name ++ " is logging in"
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
