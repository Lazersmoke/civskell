{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Civskell is a highly-configurable Minecraft server.
-- To use it, create a @'Configuration'@ and pass it in to @'runServer'@ to start the server.
module Civskell (module Civskell.Data.Types, runServer) where

import Control.Concurrent (forkIO,threadDelay)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Semigroup
import qualified Data.Set as Set
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import System.Exit (exitSuccess)
import System.IO (hSetBuffering,BufferMode(NoBuffering))
import qualified Network as Net
import Data.SuchThat
import qualified Data.Array.IArray as Arr
import Control.Lens

import Civskell.Data.Logging
import Civskell.Data.Types
import Civskell.Tech.Network
import qualified Civskell.Block.Stone as Stone
import qualified Civskell.Window as Window
import qualified Civskell.Packet.Clientbound as Client

-- | Top level entry point to the server. 
-- Run this command from your executable to run a Civskell server.
-- Note that this will *not* return until the server dies, so you should fork
-- if you want to do anything else on the main thread.
runServer :: Configuration -> IO ()
runServer c = runM . runReader c $ do
  -- This TQueue will live on its own thread and eat up all the log messages we send it from other threads.
  logger <- freshLogQueue
  _ <- send $ forkIO (loggingThread logger)
  -- Start listening for incoming connections, now with logging
  logToConsole logger (startListening logger)

-- | Initial world for testing so we don't have to chunk gen or do persistence yet.
-- This is a world with stone in the 7 chunk square around 0,0 from bedrock to halfway up.
testInitWorld :: WorldData
testInitWorld = worldChunks .~ Map.fromList [(ChunkCoord (cx,cy,cz),exampleChunk) | cx <- [-3..3], cz <- [-3..3], cy <- [0..7]] $ initWorld
  where
    -- A chunk section full of stone
    exampleChunk = ChunkSection $ Arr.array ((0,0,0),(15,15,15)) [((x,y,z),ambiguate $ Block Stone.stone (Stone.Stone :: Stone.Stone 'AsBlock)) | x <- [0..15], y <- [0..15], z <- [0..15]]

-- Create a new player and jump into their player thread. Default player information is given here
-- We need the TQueue here to be the same one that is running in the sending thread
initPlayer :: Members '[Packeting,IO,WorldManipulation,Logging] r => TQueue (ForAny (DescribedPacket PacketSerializer)) -> Eff (PlayerManipulation ': r) a -> Eff r a
initPlayer pq e = do
  pid <- view worldNextEID <$> get
  modifyPlayerInWorld pq pid e

-- This is a natural transformation from modifying a player to modifying a specific player in the world, given the player id of that player.
modifyPlayerInWorld :: Members '[WorldManipulation,Logging] r => TQueue (ForAny (DescribedPacket PacketSerializer)) -> PlayerId -> Eff (PlayerManipulation ': r) a -> Eff r a
modifyPlayerInWorld pq pId = runNat $ \case
  Get -> fromMaybe playerData . view (worldPlayers . at pId) <$> get
  Put p' -> modify $ worldPlayers . at pId .~ Just p'
  where
    playerData = PlayerData
      {_playerTPConfirmQueue = Set.empty
      ,_playerNextTid = 0
      ,_playerKeepAliveQue = Set.empty
      ,_playerNextKid = 0
      ,_playerNextWid = 1
      ,_playerWindows = Map.fromList [(0,ambiguate $ Window Window.player Window.Player)]
      ,_playerInventory = Map.empty
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


-- Note for testing: Use virtualbox port forwarding to test locally
-- Start up the server
startListening :: Members '[Logging,Configured,IO] r => LogQueue -> Eff r ()
startListening lq = do
  -- Make a new MVar for the World, starting with the test world
  -- TODO: Add config option for world-gen vs set world
  wor <- send $ newTVarIO testInitWorld
  -- bad fork
  (c :: Configuration) <- ask
  -- Fork a new thread to wait for incoming connections, and send the Socket from each new connection to `connLoop`.
  _ <- send . forkIO . runM . runReader c . logToConsole lq . runWorld wor . connLoop wor lq =<< send (Net.listenOn (Net.PortNumber 25565))
  -- Fork a thread to periodically send keep alives to everyone
  _ <- send . forkIO . runM . runReader c . logToConsole lq . runWorld wor $ keepAliveThread 0
  -- Listen for the console to say to quit. The main thread is retired to terminal duty
  send terminal

-- Send a keep alive packet to every player every 2 seconds
keepAliveThread :: Members '[Logging,WorldManipulation,IO] r => KeepAliveId -> Eff r ()
keepAliveThread i = do
  -- Wait 20 seconds
  send (threadDelay 20000000)
  -- Send everyone a keep alive packet
  broadcastPacket $ DescribedPacket (Client.keepAlive 0x1F) (Client.KeepAlive i)
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
connLoop :: Members '[Logging,WorldManipulation,Configured,IO] r => TVar WorldData -> LogQueue -> Net.Socket -> Eff r ()
connLoop wor lq sock = do
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
  (c :: Configuration) <- ask
  -- Getting thread
  _ <- send . forkIO . runM . runReader c . logToConsole lq . runNetworking mEnc mThresh handle . runWorld wor . runPacketing . initPlayer pq $ packetLoop
  -- Sending thread
  _ <- send . forkIO . runM . runReader c . logToConsole lq . runNetworking mEnc mThresh handle . runPacketing $ flushPackets pq
  -- Wait for another connection
  connLoop wor lq sock

-- TODO: remove PerformsIO constraint in favor of special STM effects or something
flushPackets :: Members '[Logging,Packeting,IO] r => TQueue (ForAny (DescribedPacket PacketSerializer)) -> Eff r ()
flushPackets q = send (atomically $ readTQueue q) >>= (\(SuchThat (DescribedPacket d p)) -> sendPacket d p) >> flushPackets q

-- Get packet -> Spawn thread -> Repeat loop for players
packetLoop :: Members '[Packeting,IO,Configured,PlayerManipulation,WorldManipulation,Logging,Networking] r => Eff r ()
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
      _ -> error "Unimplemented: ParThreading"
      --ParThreading -> send . (>> pure ()) . forkIO =<< (runM <$>) . forkConfig =<< forkLogger =<< forkNetwork =<< forkWorld =<< (runPacketing <$> forkPlayer ((onPacket . packetHandler $ pktDesc) q))
  packetLoop
  where
    -- This is an *action* to decide what parser to use. It needs to be like this because
    -- the player could change states *while we are waiting* for the next packet to arrive.
    -- This design ensures that the correct parser is always used.
    getPacketParsers = do
      ss <- view playerState <$> get
      getPackets <- packetsForState <$> ask
      pure (getPackets ss)
