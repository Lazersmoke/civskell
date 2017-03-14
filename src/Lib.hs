{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import Control.Concurrent (forkIO,threadDelay)
import Data.Functor.Identity
import Control.Concurrent.MVar (MVar,newMVar)
import Control.Eff (Eff,runM,send)
import Control.Monad (forM_,when)
import Data.List (intercalate)
import System.Exit (exitSuccess)
import System.IO (hSetBuffering,BufferMode(NoBuffering))
import qualified Data.ByteString as BS
import qualified Network as Net
import Data.SuchThat

import Civskell.Data.Logging
import Civskell.Data.Player
import Civskell.Data.Types
import Civskell.Data.World
import Civskell.Tech.Encrypt
import Civskell.Tech.Network
import qualified Civskell.Packet.Clientbound as Client
import qualified Civskell.Packet.Serverbound as Server

-- Send new connections to connLoop, and listen on 25565
-- Note for testing: Use virtualbox port forwarding to test locally
startListening :: IO ()
startListening = do
  -- Make an MVar for the global state. Use test version for the flat stone plains
  wor <- newMVar testInitWorld
  -- Make an MVar for locking the logger
  lock <- newMVar ()
  -- Fork a new thread to wait for incoming connections, and pass the world reference to it
  _ <- forkIO (connLoop wor lock =<< Net.listenOn (Net.PortNumber 25565))
  _ <- forkIO (runM $ runLogger lock $ runWorld wor $ keepAliveThread 0)
  -- Listen for the console to say to quit
  terminal

keepAliveThread :: (HasLogging r, HasWorld r,HasIO r) => KeepAliveId -> Eff r ()
keepAliveThread i = do
  send (threadDelay 2000000)
  broadcastPacket (Client.KeepAlive i)
  logLevel VerboseLog "Broadcasting Keepalives"
  keepAliveThread (i + 1)

--tickThread :: (HasWorld r,HasLogging r, HasIO r) => Eff r ()
--tickThread = arbitraryWorld $ \w -> do
  --forallPlayers tickPlayer
  --send (threadDelay 50000)
  --tickThread

--tickPlayer :: PlayerInfo -> PlayerInfo
--tickPlayer p = p {diggingBlocks = fmap b (diggingBlocks p)}
  --where
    --b (InProgress 20) = DoneBreaking
    --b (InProgress n) = InProgress (n + 1)
    --b DoneBreaking = DoneBreaking
-- This is a hack; make something better in the future
-- Note that World effect uses MVars safely enough that we can use the console with it and be OK
terminal :: IO ()
terminal = do
  l <- getLine
  if l == "quit" then exitSuccess else terminal

-- Spawns a new thread to deal with each new client
connLoop :: MVar WorldData -> MVar () -> Net.Socket -> IO ()
connLoop wor lock sock = do
  -- Accept is what waits for a new connection
  (handle, cliHost, cliPort) <- Net.accept sock
  -- Log that we got a connection
  putStrLn $ "Got Client connection: " ++ show cliHost ++ ":" ++ show cliPort
  -- Don't line buffer the network
  hSetBuffering handle NoBuffering
  -- We don't need the thread id
  _ <- forkIO (runM $ runLogger lock $ runNetworking Nothing Nothing handle $ runWorld wor $ connHandler)
  -- Wait for another connection
  connLoop wor lock sock

-- Wait for a Handshake, and hand it off when we get it
connHandler :: (HasLogging r,HasIO r,HasNetworking r,HasWorld r) => Eff r ()
connHandler = getPacket @Server.Handshake >>= maybe (loge "WTF no packet? REEEEEEEEEEEEE") handleHandshake

handleHandshake :: (HasLogging r,HasIO r,HasNetworking r,HasWorld r) => Server.Handshake -> Eff r ()
-- Normal handshake recieved
handleHandshake (Server.Handshake protocol _addr _port newstate) = if fromIntegral protocol == protocolVersion
  -- They are using the correct protocol version, so continue as they request
  then case newstate of
    -- 1 for status
    1 -> statusMode
    -- 2 for login
    2 -> initiateLogin
    -- Otherwise, drop the connection by returning
    -- TODO: Enumerate handshake options
    _ -> return ()
  -- They are using the incorrect protocol version. Disconnect packet will probably work even if they have a different version.
  else sendPacket (Client.Disconnect (jsonyText $ "Unsupported protocol version. Please use " ++ show protocolVersion))
-- Legacy response
handleLegacyHandshake :: HasNetworking r => Server.LegacyHandshake -> Eff r ()
handleLegacyHandshake Server.LegacyHandshake = rPut $ BS.pack [0xFF,0x00,0x1b,0x00,0xa7
  ,0x00,0x31 -- 1
  ,0x00,0x00 -- Seperator
  ,0x00,0x33 -- 3
  ,0x00,0x31 -- 1
  ,0x00,0x36 -- 6
  ,0x00,0x00 -- Seperator
  ,0x00,0x31 -- 1
  ,0x00,0x2e -- .
  ,0x00,0x31 -- 1
  ,0x00,0x31 -- 1
  ,0x00,0x2e -- .
  ,0x00,0x32 -- 2
  ,0x00,0x00 -- Seperator
  ,0x00,0x43 -- C
  ,0x00,0x69 -- i
  ,0x00,0x76 -- v
  ,0x00,0x73 -- s
  ,0x00,0x6b -- k
  ,0x00,0x65 -- e
  ,0x00,0x6c -- l
  ,0x00,0x6c -- l
  ,0x00,0x00 -- Seperator
  ,0x00,0x00 -- 0
  ,0x00,0x00 -- Seperator
  ,0x00,0x00 -- 0
  ]
-- If its not a handshake packet, Log to console that we have a bad packet
-- Drop the connection by returning, since the client can't receive normal disconnect packets yet
--handleHandshake (ServerPacket (p::a)) = loge $ "Unexpected non-handshake packet with Id: " ++ show (packetId @a)

encryptionPhase :: (HasIO r,HasLogging r,HasNetworking r,HasWorld r) => String -> Eff r ()
encryptionPhase nameFromLoginStart = do
  -- Get a new keypair to use with this client during the key exchange
  -- TODO: globalize this keypair so we can remove the HasIO constraint
  (pub,priv) <- getAKeypair
  -- Verify Token is fixed because why not
  -- TODO: make this a random token
  let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
  -- Server Id is blank because (((history)))
  let sId = ""
  -- Encode our public key into ASN.1's serialized format
  let encPubKey = encodePubKey pub
  -- Send an encryption request to the client
  sendPacket (Client.EncryptionRequest sId encPubKey vt)
  -- Wait for them to send an Encryption Response
  getPacket @Server.EncryptionResponse >>= \case
    Just (Server.EncryptionResponse ssFromClient vtFromClient) -> do
      -- Make sure that the encryption stuff all lines up properly
      case checkVTandSS priv vtFromClient ssFromClient vt of
        -- If it doesn't, disconnect
        Left s -> sendPacket (Client.Disconnect (jsonyText s)) >> loge s
        -- If it does, keep going
        Right ss -> do
          -- Start encrypting our packets, now that we have the shared secret
          setupEncryption (makeEncrypter ss, ss, ss)
          -- Make the serverId hash for auth
          let loginHash = genLoginHash sId ss encPubKey
          authPhase nameFromLoginStart loginHash
    Nothing -> do
      loge "Bad EncryptionResponse packet"
      sendPacket (Client.Disconnect (jsonyText "Bad EncryptionResponse packet"))

authPhase :: (HasIO r,HasLogging r,HasNetworking r,HasWorld r) => String -> String -> Eff r ()
authPhase name hash = do
  -- Do the Auth stuff with Mojang
  -- TODO: This uses arbitrary IO, we should make it into an effect
  authGetReq name hash >>= \case
    -- If the auth is borked, its probably Mojangs fault tbh
    Nothing -> do
      loge "Parse error on auth"
      -- Disclaim guilt
      sendPacket (Client.Disconnect $ jsonyText "Auth failed (not Lazersmoke's fault, probably!)")
    Just (uuid,nameFromAuth) -> initPlayer $ do
      setUsername nameFromAuth
      setUUID uuid
      -- Tell the client to compress starting now. Agressive for testing
      -- EDIT: setting this to 3 gave us a bad frame exception :S
      -- TODO: merge this packet into `setCompression`. setCompression is already a Networking effect, so this is ok
      sendPacket (Client.SetCompression 16)
      setCompression $ Just 16
      -- Send a login success. We are now in play mode
      sendPacket (Client.LoginSuccess uuid nameFromAuth)
      startPlaying

-- Start Play Mode, where we have just sent "Login Success"
startPlaying :: (HasPlayer r,HasWorld r,HasLogging r,HasNetworking r) => Eff r ()
startPlaying = do
  -- First 0 is player's EID, second is the dimension (overworld), 100 is max players
  -- TODO: newtype for EID, enum for dimension
  sendPacket (Client.JoinGame 0 Survival 0 Peaceful 100 "default" False)
  setGamemode Survival
  -- Tell them we aren't vanilla so no one gets mad at mojang for our fuck ups
  sendPacket (Client.PluginMessage "MC|Brand" (serialize "Civskell"))
  -- Difficulty to peaceful (TODO: eventually a config file for these things)
  sendPacket (Client.ServerDifficulty Peaceful)
  -- World Spawn/Compass Direction, not where they will spawn initially
  sendPacket (Client.SpawnPosition (Block (0,64,0)))
  -- Player Abilities
  -- Sent in setGamemode Survival
  -- sendPacket (Client.PlayerAbilities (AbilityFlags False False False False) 0.0 1.0)
  -- Send initial world. Need a 7x7 grid or the client gets angry with us
  forM_ [0..48] $ \x -> sendPacket =<< colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
  -- Send an initial blank inventory
  sendPacket (Client.WindowItems 0 (replicate 45 EmptySlot))
  -- Give them some stone (for testing)
  setInventorySlot 4 (Slot 1 32 0 Nothing)
  -- Start the main packet response loop
  packetLoop

packetLoop :: (HasPlayer r,HasLogging r,HasNetworking r,HasWorld r) => Eff r ()
packetLoop = do
  -- If there is a packet ready, get the packet and act on it
  r <- isPacketReady
  when r $ getGenericPacket Server.parsePlayPacket >>= maybe (loge "Failed to parse incoming packet") (ambiguously (onPacket . runIdentity))
  -- Either way, flush the inbox and recurse after we are done
  flushInbox
  packetLoop

{-gotPacket :: (HasNetworking r,HasPlayer r,HasLogging r,HasWorld r) => ServerPacket 'Playing -> Eff r ()
-- If the packet isn't matched above, log an error. We were able to parse it, just haven't implemented anything to handle it
gotPacket a = loge $ "Unsupported packet: " ++ show a
-}

-- First, check if the slot matches the client provided one

-- Do the login process
initiateLogin :: (HasIO r,HasLogging r,HasNetworking r,HasWorld r) => Eff r ()
initiateLogin = do
  -- Get a login start packet from the client
  getPacket @Server.LoginStart >>= \case
    -- LoginStart packets contain their username as a String
    Just (Server.LoginStart name) -> do
      -- Log that they are logging in
      logt name "Logging In"
      encryptionPhase name
    Nothing -> do
      loge "Bad LoginStart packet"
      -- We can send friendly disconnect messages since we are in the "Login" phase
      sendPacket (Client.Disconnect (jsonyText "Bad LoginStart packet"))

-- Server list refresh ping/status cycle
statusMode :: (HasLogging r,HasNetworking r,HasWorld r) => Eff r ()
statusMode = do
  -- Wait for them to ask our status
  getPacket @Server.StatusRequest >>= \case
    -- Client gave us an unparseable packet, so disconnect with an error
    Nothing -> loge "Client gave nonsense packet, disconnecting"
    -- Make sure they are actually asking for our status
    Just Server.StatusRequest -> do
      -- Get the list of connected players so we can show info about them in the server menu
      playersOnline <- allPlayers
      let userSample = intercalate "," . map (\p -> "{\"name\":\"" ++ clientUsername p ++ "\",\"id\":\"" ++ clientUUID p ++ "\"}") $ playersOnline
      let resp = "{\"version\":{\"name\":\"Civskell (1.11.2)\",\"protocol\":" ++ show protocolVersion ++ "},\"players\":{\"max\": 100,\"online\": " ++ show (length playersOnline) ++ ",\"sample\":[" ++ userSample ++ "]},\"description\":{\"text\":\"An Experimental Minecraft server written in Haskell | github.com/Lazersmoke/civskell\"},\"favicon\":\"" ++ image ++ "\"}"
      -- Send resp to clients
      sendPacket (Client.StatusResponse resp)
      -- Wait for them to start a ping
      getPacket @Server.StatusPing >>= \case
        -- Make sure they are actually pinging us, then send a pong right away
        Just (Server.StatusPing l) -> sendPacket (Client.StatusPong l)
        Nothing -> loge "Client gave nonsense packet instead of ping, disconnecting"

-- TODO: make this not cancer
image :: String
image = "data:image/png;base64,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"
