{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Civskell (module Civskell.Data.Types, runServer) where

import Control.Concurrent (forkIO,threadDelay)
import Data.Functor.Identity
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
import qualified Civskell.Tile as Tile
import qualified Civskell.Window as Window
import qualified Civskell.Packet.Clientbound as Client
import qualified Civskell.Packet.Serverbound as Server

runServer :: Configuration -> IO ()
runServer c = runM $ runReader' c startListening

testInitWorld :: WorldData
testInitWorld = initWorld {chunks = Map.fromList [(ChunkCoord (cx,cy,cz),exampleChunk) | cx <- [-3..3], cz <- [-3..3], cy <- [0..7]]}
  where
    exampleChunk = ChunkSection $ Map.fromList [(BlockLocation (x,y,z),some Tile.Stone) | x <- [0..15], y <- [0..15], z <- [0..15]]

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
      ,clientUsername = undefined
      ,clientBrand = undefined
      ,clientUUID = undefined
      ,playerId = undefined
      ,packetQueue = pq
      }

-- Send new connections to connLoop, and listen on 25565
-- Note for testing: Use virtualbox port forwarding to test locally
startListening :: (Configured r,PerformsIO r) => Eff r ()
startListening = do
  -- Make an MVar for the global state. Use test version for the flat stone plains
  wor <- send $ newMVar testInitWorld
  -- Make a TQueue for logging
  logger <- send (newTQueueIO :: IO (TQueue String))
  -- bad fork
  (c :: Configuration) <- ask
  -- Fork a new thread to wait for incoming connections, and pass the world reference to it
  _ <- send . forkIO . runM . runReader' c . runLogger logger . runWorld wor . connLoop =<< send (Net.listenOn (Net.PortNumber 25565))
  -- Fork a thread to periodically send keep alives to everyone
  _ <- send . forkIO . runM =<< (forkConfig . runLogger logger . runWorld wor $ keepAliveThread 0)
  -- Fork a thread to continuously log every log message
  _ <- send $ forkIO (loggingThread logger)
  -- Listen for the console to say to quit
  send $ terminal

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
connLoop :: (Logs r,HasWorld r,Configured r,PerformsIO r) => Net.Socket -> Eff r ()
connLoop sock = do
  -- Accept is what waits for a new connection
  (handle, cliHost, cliPort) <- send $ Net.accept sock
  -- Log that we got a connection
  logg $ "Got Client connection: " ++ show cliHost ++ ":" ++ show cliPort
  -- Don't line buffer the network
  send $ hSetBuffering handle NoBuffering
  -- Make new networking TVars for this client
  netLock <- send $ newMVar ()
  mEnc <- send $ newTVarIO Nothing
  mThresh <- send $ newTVarIO Nothing
  -- Packet TQueue
  pq <- send $ newTQueueIO
  -- Getting thread
  _ <- send . forkIO . runM =<< forkConfig =<< (forkLogger . runNetworking netLock mEnc mThresh handle =<< (forkWorld . runPacketing . initPlayer pq $ packetLoop))
  -- Sending thread
  _ <- send . forkIO . runM =<< forkConfig =<< (forkLogger . runNetworking netLock mEnc mThresh handle . runPacketing $ flushPackets pq)
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
    Just (SuchThat (InboundPacket (SuchThat (Identity q)))) -> if canPokePacketState q
      -- Serial mode
      then onPacket q
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
      Handshaking -> ambiguate <$> Server.parseHandshakePacket
      LoggingIn -> ambiguate <$> Server.parseLoginPacket
      Status -> ambiguate <$> Server.parseStatusPacket
      Playing -> ambiguate <$> Server.parsePlayPacket
