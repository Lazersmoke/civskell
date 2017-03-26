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
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Eff (Eff,runM,send)
import System.Exit (exitSuccess)
import System.IO (hSetBuffering,BufferMode(NoBuffering))
import qualified Network as Net
import Data.SuchThat

import Civskell.Data.Logging
import Civskell.Data.Player
import Civskell.Data.Types
import Civskell.Data.World
import Civskell.Tech.Network
import qualified Civskell.Packet.Clientbound as Client
import qualified Civskell.Packet.Serverbound as Server

-- Send new connections to connLoop, and listen on 25565
-- Note for testing: Use virtualbox port forwarding to test locally
startListening :: IO ()
startListening = do
  -- Make an MVar for the global state. Use test version for the flat stone plains
  wor <- newMVar testInitWorld
  -- Make a TQueue for logging
  logger <- newTQueueIO :: IO (TQueue String)
  -- Fork a new thread to wait for incoming connections, and pass the world reference to it
  _ <- forkIO (connLoop wor logger =<< Net.listenOn (Net.PortNumber 25565))
  -- Fork a thread to periodically send keep alives to everyone
  _ <- forkIO (runM $ runLogger logger $ runWorld wor $ keepAliveThread 0)
  -- Fork a thread to continuously log every log message
  _ <- forkIO (loggingThread logger)
  -- Listen for the console to say to quit
  terminal

keepAliveThread :: (Logs r, HasWorld r,PerformsIO r) => KeepAliveId -> Eff r ()
keepAliveThread i = do
  send (threadDelay 2000000)
  broadcastPacket (Client.KeepAlive i)
  logLevel VerboseLog "Broadcasting Keepalives"
  keepAliveThread (i + 1)

loggingThread :: TQueue String -> IO ()
loggingThread l = atomically (readTQueue l) >>= putStrLn >> loggingThread l

-- This is a hack; make something better in the future
-- Note that World effect uses MVars safely enough that we can use the console with it and be OK
terminal :: IO ()
terminal = do
  l <- getLine
  if l == "quit" then exitSuccess else terminal

-- Spawns a new thread to deal with each new client
connLoop :: MVar WorldData -> TQueue String -> Net.Socket -> IO ()
connLoop wor logger sock = do
  -- Accept is what waits for a new connection
  (handle, cliHost, cliPort) <- Net.accept sock
  -- Log that we got a connection
  putStrLn $ "Got Client connection: " ++ show cliHost ++ ":" ++ show cliPort
  -- Don't line buffer the network
  hSetBuffering handle NoBuffering
  -- Make new networking TVars for this client
  netLock <- newMVar ()
  mEnc <- newTVarIO Nothing
  mThresh <- newTVarIO Nothing
  -- Packet TQueue
  pq <- newTQueueIO
  -- Getting thread
  _ <- forkIO (runM $ runLogger logger $ runNetworking netLock mEnc mThresh handle $ runWorld wor $ runPacketing $ initPlayer pq $ packetLoop)
  -- Sending thread
  _ <- forkIO (runM $ runLogger logger $ runNetworking netLock mEnc mThresh handle $ runPacketing $ flushPackets pq)
  -- Wait for another connection
  connLoop wor logger sock

-- TODO: remove PerformsIO constraint in favor of special STM effects or something
flushPackets :: (Logs r,SendsPackets r,PerformsIO r) => TQueue (ForAny ClientPacket) -> Eff r ()
flushPackets q = send (atomically $ readTQueue q) >>= sendAnyPacket >> flushPackets q

-- Get packet -> Spawn thread -> Repeat loop for players
packetLoop :: (PerformsIO r,HasPlayer r,Logs r,Networks r,HasWorld r) => Eff r ()
packetLoop = do
  -- TODO: Fork new thread on every packet
  -- Block until a packet arrives, then deal with it
  getGenericPacket getParser >>= \case
    Nothing -> loge "Failed to parse incoming packet"
    -- If you are chasing a bug and you think it may be caused by the one-liner below, you are probably right.
    -- You can't substitute `onPacket q` for `op` except using the let binding as shown below. The type gods
    -- will become angry and smite you where you stand. TODO: investigate appeasing the type gods by enabling/
    -- disabling the monomorphism restriction
    Just (SuchThat (ServerPacket (SuchThat (Identity q)))) -> do
      let op = onPacket q
      send . (>> pure ()) . forkIO =<< (runM <$>) . forkLogger =<< forkNetwork =<< forkWorld =<< (runPacketing <$> forkPlayer op)
  packetLoop
  where
    -- This is an *action* to decide what parser to use. It needs to be like this because
    -- the player could change states *while we are waiting* for the next packet to arrive.
    -- This design ensures that the correct parser is always used.
    getParser = flip fmap (playerState <$> getPlayer) $ \case
      Handshaking -> ambiguate <$> Server.parseHandshakePacket
      LoggingIn -> ambiguate <$> Server.parseLoginPacket
      Status -> ambiguate <$> Server.parseStatusPacket
      Playing -> ambiguate <$> Server.parsePlayPacket
