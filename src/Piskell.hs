{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Piskell where

import qualified Network as Net
import Data.Functor.Identity
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import Data.SuchThat
import Data.Semigroup
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Eff
import Control.Eff.State
import Control.Eff.Reader
import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import System.IO

import Civskell.Data.Types
import Civskell.Data.Logging
import Civskell.Tech.Network
import Civskell.Tech.Encrypt
import qualified Civskell.Packet.Serverbound as Server
import qualified Civskell.Packet.Clientbound as Client

type HasPiskell r = Member (State PiskellState) r

data PiskellState = Piskell
  {currentState :: ServerState}

class Packet p => PiskellPacket p where
  reactToPacket :: (SendsPackets r,HasPiskell r,PerformsIO r,Logs r) => p -> Eff r ()
  canPokePacketStatePiskell :: Bool
  canPokePacketStatePiskell = False

runClient :: Configuration -> IO ()
runClient c = runM . runReader' c $ do
  -- Make a TQueue for logging
  logger <- LogQueue <$> send (newTQueueIO :: IO (TQueue T.Text))
  -- Fork a thread to continuously log every log message
  _ <- send $ forkIO (loggingThread logger)
  -- Retire main thread to connection duty
  runLogger logger . login =<< send (Net.connectTo "localhost" (Net.PortNumber 25565))

-- Send every log message in the TQueue to putStrLn until the thread is killed
loggingThread :: LogQueue -> IO ()
loggingThread (LogQueue l) = atomically (readTQueue l) >>= T.putStrLn >> loggingThread (LogQueue l)

-- Spawns a new thread to deal with each new client
login :: (Configured r, Logs r,PerformsIO r) => Handle -> Eff r ()
login hdl = do
  -- Log that we got a connection
  logg $ "Connecting to: localhost:25565"
  -- Don't line buffer the network
  send $ hSetBuffering hdl NoBuffering
  -- Make new networking TVars for this client
  mEnc <- send $ newTVarIO Nothing
  mThresh <- send $ newTVarIO Nothing
  let setup = sendPacket (Server.Handshake 316 "localhost" 25565 2) >> sendPacket (Server.LoginStart "piskell-client")
  -- Getting thread
  _ <- send . forkIO . runM =<< forkConfig =<< (forkLogger . runNetworking mEnc mThresh hdl . runPacketing . evalState' Piskell {currentState = LoggingIn} $ setup >> packetLoop)
  runNetworking mEnc mThresh hdl . runPacketing $ terminal

terminal :: (Configured r, Logs r, PerformsIO r, SendsPackets r) => Eff r ()
terminal = send getLine >>= \case
  "a" -> sendPacket (Server.ChatMessage "Test Chat Message") >> terminal
  "q" -> pure ()
  _ -> terminal

instance PiskellPacket Client.EncryptionRequest where
  reactToPacket (Client.EncryptionRequest _sId pubKeyEnc vt) = do
    let sharedSecret = BS.pack [0x01,0x00,0x03,0x02,0x05,0x04,0x07,0x06,0x09,0x08,0x0B,0x0A,0x0D,0x0C,0x0F,0x0E]
    case runGetS (deserialize @MCPubKey) pubKeyEnc of
      Left e -> send $ print e
      Right pubKey -> do
        (vtResp,ssResp) <- genEncryptionResponse pubKey vt sharedSecret
        sendPacket (Server.EncryptionResponse ssResp vtResp)
        beginEncrypting (makeEncrypter sharedSecret,sharedSecret,sharedSecret)

instance PiskellPacket Client.LoginSuccess where
  reactToPacket (Client.LoginSuccess _uuid _username) = do
    logg "Login Successful. Switching to `Play` mode."
    modify (\x -> x {currentState = Playing})

instance PiskellPacket Client.SetCompression where
  reactToPacket (Client.SetCompression thresh) = do
    logg $ "Compression threshold set to " <> T.pack (show thresh)
    beginCompression thresh
    
  -- get Join Game
  -- get Plugin Message with Brand
  -- get Difficulty
  -- get Spawn position
  -- get plaer abilities

instance PiskellPacket Client.PlayerAbilities where
  reactToPacket (Client.PlayerAbilities _ _ _) = do
    sendPacket (Server.PluginMessage "MC|Brand" . runPutS . serialize @ProtocolString $ "Piskell Client")
    sendPacket (Server.ClientSettings "en_US" 8 {-Chunks Render distance-} 0 {-Chat enabled-} True {-Chat Colors enabled-} 0x7f {-All skin componnents enabled-} 1 {-Main hand: Right-})

instance PiskellPacket Client.PlayerPositionAndLook where
  reactToPacket (Client.PlayerPositionAndLook pos look _relFlags tpId) = do
    logg $ "Player position and look: " <> T.pack (show pos)
    sendPacket (Server.TPConfirm tpId)
    sendPacket (Server.PlayerPositionAndLook pos look True)
    sendPacket (Server.ClientStatus PerformRespawn)

instance PiskellPacket Client.Disconnect where
  reactToPacket (Client.Disconnect (ProtocolString reason)) = logg $ "Disconnect because: " <> T.pack reason

packetLoop :: (Logs r,SendsPackets r,HasPiskell r,Networks r,PerformsIO r) => Eff r ()
packetLoop = do
  -- Block until a packet arrives, then deal with it
  getGenericPacket @PiskellPacket getParser >>= \case
    Nothing -> loge "Failed to parse incoming packet"
    Just (SuchThat (Identity q)) -> reactToPacket q
  packetLoop
  where
    -- This is an *action* to decide what parser to use. It needs to be like this because
    -- the player could change states *while we are waiting* for the next packet to arrive.
    -- This design ensures that the correct parser is always used.
    getParser = flip fmap (currentState <$> get) $ \case
      LoggingIn -> piLogin
      --Status -> piStatus
      --Playing -> piPlaying
      _ -> undefined

piLogin :: MonadGet m => ParseSet m VarInt '[Packet,PiskellPacket]
piLogin = Map.fromList 
  [(packetId @Client.LoginSuccess,ambiguate . Identity <$> deserialize @Client.LoginSuccess)
  ,(packetId @Client.EncryptionRequest,ambiguate . Identity <$> deserialize @Client.EncryptionRequest)
  ,(packetId @Client.Disconnect,ambiguate . Identity <$> deserialize @Client.Disconnect)
  --,(packetId @Client.LegacyHandshake,ambiguate . Identity <$> deserialize @Client.LegacyHandshake)
  ]

