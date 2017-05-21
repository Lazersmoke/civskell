{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Civskell (module Civskell.Data.Types, runServer) where

import Control.Concurrent (forkIO,threadDelay)
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Semigroup
import qualified Data.Set as Set
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Eff (Eff,runM,send)
import Control.Eff.Reader
import System.Exit (exitSuccess)
import System.IO (hSetBuffering,BufferMode(NoBuffering))
import qualified Network as Net
import Data.SuchThat

import Civskell.Data.Logging
import Civskell.Data.Player
import Civskell.Data.Types
import Civskell.Data.World
import Civskell.Tech.Network
import qualified Civskell.Block.Stone as Stone
import qualified Civskell.Window as Window
import qualified Civskell.Packet.Clientbound as Client
import qualified Civskell.Packet.Serverbound as Server

-- Top level entry point to the server. Drops us from IO into a Configured Eff
runServer :: Configuration -> IO ()
runServer c = runM . runReader' c $ do
  -- This TQueue will live on its own thread and eat up all the log messages we send it from other threads.
  -- TODO: Add configuration for variable logging outputs here. (To console, to file, etc)
  -- TODO: Change log messages to use Text instead of String
  logger <- freshLogQueue
  -- Fork a thread to continuously log every log message
  _ <- send $ forkIO (loggingThread logger)
  -- Start listening for incoming connections, now with logging
  runLogger logger startListening

-- Initial world for testing so we don't have to chunk gen or do persistence yet.
-- This is a world with stone in the 7 chunk square around 0,0 from bedrock to halfway up.
testInitWorld :: WorldData
testInitWorld = initWorld {chunks = Map.fromList [(ChunkCoord (cx,cy,cz),exampleChunk) | cx <- [-3..3], cz <- [-3..3], cy <- [0..7]]}
  where
    -- A chunk section full of stone
    exampleChunk = ChunkSection $ Map.fromList [(BlockLocation (x,y,z),some (Stone.Stone :: Stone.Stone 'AsBlock)) | x <- [0..15], y <- [0..15], z <- [0..15]]

-- Create a new player and jump into their player thread. Default player information is given here
-- We need the TQueue here to be the same one that is running in the sending thread
initPlayer :: (SendsPackets r, PerformsIO r, HasWorld r, Logs r) => TQueue (ForAny OutboundPacket) -> Eff (Player ': r) a -> Eff r a
initPlayer pq e = do
  t <- send (newTVarIO playerData)
  runPlayer t e
  where
    playerData = PlayerData
      {teleportConfirmationQue = Set.empty
      ,nextTid = 0
      ,keepAliveQue = Set.empty
      ,nextKid = 0
      ,nextWid = 1
      ,windows = Map.fromList [(0,some Window.Player)]
      ,playerInventory = Map.empty
      ,failedTransactions = Set.empty
      ,holdingSlot = 0
      ,playerPosition = (0,0,0)
      ,viewAngle = (0,0)
      ,gameMode = Survival
      ,diggingBlocks = Map.empty
      ,moveMode = Walking
      ,playerState = Handshaking
      -- TODO: undefined is bad, don't use it
      ,clientUsername = undefined
      ,clientBrand = undefined
      ,clientUUID = undefined
      ,playerId = undefined
      ,packetQueue = pq
      }

-- Note for testing: Use virtualbox port forwarding to test locally
-- Start up the server
startListening :: (Logs r,Configured r,PerformsIO r) => Eff r ()
startListening = do
  -- Make a new MVar for the World, starting with the test world
  -- TODO: Add config option for world-gen vs set world
  wor <- send $ newMVar testInitWorld
  -- bad fork
  (c :: Configuration) <- ask
  -- Fork a new thread to wait for incoming connections, and send the Socket from each new connection to `connLoop`.
  _ <- send . forkIO . runM . runReader' c =<< (forkLogger . runWorld wor . connLoop =<< send (Net.listenOn (Net.PortNumber 25565)))
  -- Fork a thread to periodically send keep alives to everyone
  _ <- send . forkIO . runM =<< (forkConfig =<< (forkLogger . runWorld wor $ keepAliveThread 0))
  -- Listen for the console to say to quit. The main thread is retired to terminal duty
  send terminal

-- Send a keep alive packet to every player every 2 seconds
keepAliveThread :: (Logs r, HasWorld r,PerformsIO r) => KeepAliveId -> Eff r ()
keepAliveThread i = do
  -- Wait 2 seconds
  send (threadDelay 2000000)
  -- Send everyone a keep alive packet
  broadcastPacket (Client.KeepAlive i)
  logLevel VerboseLog "Broadcasting Keepalives"
  -- Do it again with the next keep alive id
  keepAliveThread (succ i)

-- Send every log message in the TQueue to putStrLn until the thread is killed
loggingThread :: LogQueue -> IO ()
loggingThread (LogQueue l) = atomically (readTQueue l) >>= T.putStrLn >> loggingThread (LogQueue l)

-- This is a hack; make something better in the future
terminal :: IO ()
terminal = do
  l <- getLine
  if l == "quit" then exitSuccess else terminal

-- Spawns a new thread to deal with each new client
connLoop :: (Logs r,HasWorld r,Configured r,PerformsIO r) => Net.Socket -> Eff r ()
connLoop sock = do
  -- Accept every connection
  -- TODO: Timeout handlers or something here
  (handle, cliHost, cliPort) <- send $ Net.accept sock
  -- Log that we got a connection
  logg $ "Got Client connection: " <> T.pack (show cliHost) <> ":" <> T.pack (show cliPort)
  -- Don't line buffer the network
  send $ hSetBuffering handle NoBuffering
  -- Make new networking TVars for this client
  mEnc <- send $ newTVarIO Nothing
  mThresh <- send $ newTVarIO Nothing
  -- Packet TQueue PLTs to send outgoing packets to. They will be fired off asynchronously in another thread
  pq <- send $ newTQueueIO
  -- Getting thread
  _ <- send . forkIO . runM =<< forkConfig =<< (forkLogger . runNetworking mEnc mThresh handle =<< (forkWorld . runPacketing . initPlayer pq $ packetLoop))
  -- Sending thread
  _ <- send . forkIO . runM =<< forkConfig =<< (forkLogger . runNetworking mEnc mThresh handle . runPacketing $ flushPackets pq)
  -- Wait for another connection
  connLoop sock

-- TODO: remove PerformsIO constraint in favor of special STM effects or something
flushPackets :: (Logs r,SendsPackets r,PerformsIO r) => TQueue (ForAny OutboundPacket) -> Eff r ()
flushPackets q = send (atomically $ readTQueue q) >>= sendAnyPacket >> flushPackets q

-- Get packet -> Spawn thread -> Repeat loop for players
packetLoop :: (Configured r,PerformsIO r,HasPlayer r,Logs r,Networks r,HasWorld r) => Eff r ()
packetLoop = do
  -- TODO: Fork new thread on every packet
  -- Block until a packet arrives, then deal with it
  getGenericPacket getParser >>= \case
    Nothing -> loge "Failed to parse incoming packet"
    -- If you are chasing a bug and you think it may be caused by the one-liner below, you are probably right.
    -- You can't substitute `onPacket q` for `op` except using the let binding as shown below. The type gods
    -- will become angry and smite you where you stand. TODO: investigate appeasing the type gods by enabling/
    -- disabling the monomorphism restriction
    Just (SuchThat (InboundPacket (SuchThat (Identity (q :: qt))))) -> if canPokePacketState @qt
      -- Serial mode
      then runPacketing $ onPacket q
      -- Parallel mode
      else do
        let op = onPacket q
        send . (>> pure ()) . forkIO =<< (runM <$>) . forkConfig =<< forkLogger =<< forkNetwork =<< forkWorld =<< (runPacketing <$> forkPlayer op)
  packetLoop
  where
    -- This is an *action* to decide what parser to use. It needs to be like this because
    -- the player could change states *while we are waiting* for the next packet to arrive.
    -- This design ensures that the correct parser is always used.
    getParser = flip fmap (playerState <$> getPlayer) $ \case
      Handshaking -> Server.parseHandshakePacket
      LoggingIn -> Server.parseLoginPacket
      Status -> Server.parseStatusPacket
      Playing -> Server.parsePlayPacket
