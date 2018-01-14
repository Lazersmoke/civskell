{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Civskell is a highly-configurable Minecraft server.
-- To use it, create a @'Configuration'@ and pass it in to @'runServer'@ to start the server.
module Civskell (module Civskell.Data.Types, runServer) where

import Control.Concurrent (forkIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Semigroup
import qualified Data.Set as Set
import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Monad.Reader
import System.Exit (exitSuccess)
import System.IO (hSetBuffering,BufferMode(NoBuffering))
import qualified Network as Net
import Data.SuchThat
import qualified Data.Vector as Vector
import qualified Data.Array.IArray as Arr
import Control.Lens

import Civskell.Data.Logging
import Civskell.Data.Types
import Civskell.Data.Networking
import Civskell.Tech.Network
import qualified Civskell.Block.Stone as Stone
import qualified Civskell.Window as Window

-- | Top level entry point to the server. 
-- Run this command from your executable to run a Civskell server.
-- Note that this will *not* return until the server dies, so you should fork
-- if you want to do anything else on the main thread.
runServer :: Configuration -> IO ()
runServer c = do
  -- This TQueue will live on its own thread and eat up all the log messages we send it from other threads.
  logger <- freshLogQueue
  _ <- forkIO (loggingThread logger)
  -- Log our configuration on startup
  logToQueue c logger NormalLog $ displayConfig c
  -- Make a new TVar for the World, starting with the test world
  -- TODO: Add config option for world-gen vs set world
  wor <- newTVarIO testInitWorld
  -- Note for testing: Use virtualbox port forwarding to test locally
  -- Fork a new thread to wait for incoming connections, and send the Socket from each new connection to `connLoop`.
  _ <- forkIO . connLoop c wor logger =<< Net.listenOn (serverPort c)
  -- Fork a thread to periodically send keep alives to everyone
  _ <- forkIO $ runReaderT (keepAliveThread c) (fakeGlobalContext c wor logger) *> pure ()
  -- Listen for the console to say to quit. The main thread is retired to terminal duty
  terminal

-- | Initial world for testing so we don't have to chunk gen or do persistence yet.
-- This is a world with stone in the 7 chunk square around 0,0 from bedrock to halfway up.
testInitWorld :: WorldData
testInitWorld = worldChunks .~ Map.fromList [(ChunkCoord (cx,cy,cz),exampleChunk) | cx <- [-3..3], cz <- [-3..3], cy <- [0..7]] $ initWorld
  where
    -- A chunk section full of stone
    exampleChunk = ChunkSection $ Arr.array ((0,0,0),(15,15,15)) [((x,y,z),ambiguate $ Block Stone.stone (Stone.Stone :: Stone.Stone 'AsBlock)) | x <- [0..15], y <- [0..15], z <- [0..15]]

defaultPlayerData :: TQueue (ForAny (DescribedPacket PacketSerializer)) -> PlayerData
defaultPlayerData pq = PlayerData
  {_playerTPConfirmQueue = Set.empty
  ,_playerNextTid = 0
  ,_playerKeepAliveQue = Set.empty
  ,_playerNextKid = 0
  ,_playerNextWid = 1
  ,_playerWindows = Map.fromList [(0,ambiguate $ Window Window.player Window.Player)]
  ,_playerInventory = Vector.replicate 46 (ambiguate $ Slot Nothing)
  ,_playerFailedTransactions = Set.empty
  ,_playerHoldingSlot = 0
  ,_playerPosition = (0,0,0)
  ,_playerViewAngle = (0,0)
  ,_playerGamemode = Survival
  ,_playerDiggingBlocks = Map.empty
  ,_playerMoveMode = Walking
  ,_playerState = Handshaking
  -- TODO: undefined is bad, don't use it
  ,_playerUsername = ""
  ,_playerClientBrand = undefined
  ,_playerClientUUID = undefined
  ,_playerId = undefined
  ,_playerPacketQueue = pq
  }
{-
-- This is a natural transformation from modifying a player to modifying a specific player in the world, given the player id of that player.
modifyPlayerInWorld :: TQueue (ForAny (DescribedPacket PacketSerializer)) -> PlayerId -> Eff (PlayerManipulation ': r) a -> Civskell a
modifyPlayerInWorld pq pId = runNat $ \case
  Get -> fromMaybe playerData . view (worldPlayers . at pId) <$> get
  Put p' -> modify $ worldPlayers . at pId .~ Just p'
-}
-- Send every log message in the TQueue to putStrLn until the thread is killed
loggingThread :: LogQueue -> IO ()
loggingThread (LogQueue l) = atomically (readTQueue l) >>= T.putStrLn >> loggingThread (LogQueue l)

-- This is a hack; make something better in the future
terminal :: IO ()
terminal = do
  l <- getLine
  if l == "quit" then exitSuccess else terminal

-- Spawns a new thread to deal with each new client
connLoop :: Configuration -> TVar WorldData -> LogQueue -> Net.Socket -> IO ()
connLoop c wor lq sock = do
  logToQueue c lq NormalLog $ "Waiting for client to connect"
  -- Accept every connection
  -- TODO: Timeout handlers or something here
  (handle, cliHost, cliPort) <- Net.accept sock
  -- Log that we got a connection
  logToQueue c lq NormalLog $ "Got Client connection: " <> T.pack (show cliHost) <> ":" <> T.pack (show cliPort)
  -- Don't line buffer the network
  hSetBuffering handle NoBuffering

  -- Setup the client in a single transaction:
  context <- atomically $ do
    -- Make a fresh context for this client:
    -- Networking TVars
    mThresh <- newTVar Nothing
    mEnc <- newTVar Nothing
    -- Packet Queue
    pq <- newTQueue
    -- Player Data TVar
    pTvar <- newTVar $ defaultPlayerData pq
    pure CivskellContext
      {configuration = c
      ,worldData = wor
      ,globalLogQueue = lq
      ,playerData = pTvar
      ,networkCompressionThreshold = mThresh
      ,networkEncryptionCouplet = mEnc
      ,networkHandle = handle
      }

  -- Getting thread
  _ <- forkIO $ runReaderT packetLoop context
  -- Sending thread
  _ <- forkIO $ runReaderT (flushPackets =<< view playerPacketQueue <$> fromContext playerData) context
  -- Wait for another connection
  connLoop c wor lq sock

-- TODO: remove PerformsIO constraint in favor of special STM effects or something
flushPackets :: TQueue (ForAny (DescribedPacket PacketSerializer)) -> Civskell ()
flushPackets q = lift (atomically $ readTQueue q) >>= (\(SuchThat (DescribedPacket d p)) -> sendPacket d p) >> flushPackets q

-- Get packet -> Spawn thread -> Repeat loop for players
packetLoop :: Civskell ()
packetLoop = do
  -- TODO: Fork new thread on every packet
  -- Block until a packet arrives, then deal with it
  getPacket getPacketParsers >>= \case
    Nothing -> loge "Failed to parse incoming packet"
    Just (SuchThat (DescribedPacket pktDesc (q :: qt))) -> case packetThreadingMode (packetHandler pktDesc) of
      -- Invariant: If a packet handler is capable of changing the ServeState,
      -- it must also be marked as SerialThreading. Otherwise, it might get a
      -- forked thread, and we get a packet from the new ServerState before the
      -- original packet finishes processing and updating the ServerState.
      SerialThreading -> (onPacket . packetHandler $ pktDesc) q
      ParThreading -> do
        ctx <- ask 
        _ <- lift . forkIO . flip runReaderT ctx $ (onPacket . packetHandler $ pktDesc) q
        pure ()
  packetLoop
  where
    -- This is an *action* to decide what parser to use. It needs to be like this because
    -- the player could change states *while we are waiting* for the next packet to arrive.
    -- This design ensures that the correct parser is always used.
    getPacketParsers = do
      ss <- view playerState <$> fromContext playerData
      getPackets <- packetsForState <$> asks configuration
      pure (getPackets ss)
