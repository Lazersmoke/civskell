{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Piskell where

import qualified Network as Net
--import Data.Functor.Identity
import Data.Bytes.Serial
--import Data.Bytes.Put
--import Data.Bytes.Get
--import Data.SuchThat
import Data.Semigroup
--import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Eff
import Control.Eff.State
import Control.Eff.Reader
--import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import System.IO

import Civskell.Data.Types
import Civskell.Data.Logging
import Civskell.Tech.Network
--import Civskell.Tech.Encrypt
import qualified Civskell.Packet.Serverbound as Server
--import qualified Civskell.Packet.Clientbound as Client

type HasPiskell r = Member (State PiskellState) r

data PiskellState = Piskell
  {currentState :: ServerState
  ,piskellUsername :: String
  ,piskellPassword :: String
  }

data PiskellPacketHandler p = PiskellPacketHandler
  {reactToPacket :: forall r. (SendsPackets r,HasPiskell r,PerformsIO r,Logs r) => p -> Eff r ()
  ,piskellThreadingMode :: Bool
  }

runClient :: Configuration -> IO ()
runClient c = runM . runReader' c $ do
  -- Get auth info
  send $ hSetBuffering stdout NoBuffering
  theUsername <- send $ putStr "Username:" >> getLine
  thePassword <- send $ putStr "Password:" >> getLine
  -- Make a TQueue for logging
  logger <- LogQueue <$> send (newTQueueIO :: IO (TQueue T.Text))
  -- Fork a thread to continuously log every log message
  _ <- send $ forkIO (loggingThread logger)
  -- Retire main thread to connection duty
  runLogger logger . login (theUsername,thePassword) =<< send (Net.connectTo "localhost" (Net.PortNumber 25565))

-- Send every log message in the TQueue to putStrLn until the thread is killed
loggingThread :: LogQueue -> IO ()
loggingThread (LogQueue l) = atomically (readTQueue l) >>= T.putStrLn >> loggingThread (LogQueue l)

-- Spawns a new thread to deal with each new client
login :: (Configured r, Logs r,PerformsIO r) => (String,String) -> Handle -> Eff r ()
login authInfo@(theUsername,thePassword) hdl = do
  mCliAuthResp <- clientAuthentication authInfo
  logg $ "Auth Response: " <> T.pack (show mCliAuthResp)
  -- Log that we got a connection
  logg $ "Connecting to: localhost:25565"
  -- Don't line buffer the network
  send $ hSetBuffering hdl NoBuffering
  -- Make new networking TVars for this client
  mEnc <- send $ newTVarIO Nothing
  mThresh <- send $ newTVarIO Nothing
  let setup = sendPacket (asSender Server.handshake) (Server.Handshake 316 "localhost" 25565 2) >> sendPacket (asSender Server.loginStart) (Server.LoginStart $ ProtocolString theUsername)
  -- Getting thread
  _ <- send . forkIO . runM =<< forkConfig =<< (forkLogger . runNetworking mEnc mThresh hdl . runPacketing . evalState' Piskell {piskellUsername = theUsername, piskellPassword = thePassword, currentState = LoggingIn} $ setup >> packetLoop)
  runNetworking mEnc mThresh hdl . runPacketing $ terminal

-- Simple busy work thread that will terminate when the user says to t
terminal :: (Configured r, Logs r, PerformsIO r, SendsPackets r) => Eff r ()
terminal = send getLine >>= \case
  "a" -> sendPacket (asSender Server.chatMessage) (Server.ChatMessage "Test Chat Message") >> terminal
  "q" -> pure ()
  _ -> terminal

asSender :: Serial p => InboundPacketDescriptor p -> OutboundPacketDescriptor p
asSender r = r {packetHandler = defaultSerializer (error "Piskell.asSender: This was written before packet ids were inferred from their position in the vector, so this method needs a rethink :thinking:")}

packetLoop :: Eff r ()
packetLoop = undefined
{-
-- When we get an encryption request, respond and enable encryption
instance PiskellReaction Client.EncryptionRequest where {piskellReaction = piskellHandleEncryptionRequest}
piskellHandleEncryptionRequest :: PiskellPacketHandler Client.EncryptionRequest
piskellHandleEncryptionRequest = PiskellPacketHandler
  {reactToPacket = \(Client.EncryptionRequest _sId pubKeyEnc vt) -> do
    -- TODO: Randomize
    let sharedSecret = BS.pack [0x01,0x00,0x03,0x02,0x05,0x04,0x07,0x06,0x09,0x08,0x0B,0x0A,0x0D,0x0C,0x0F,0x0E]
    -- Extract the public key from the encryption request
    case runGetS (deserialize @MCPubKey) pubKeyEnc of
      -- If the public key is malformed, print what went wrong
      Left e -> send $ print e
      Right pubKey -> do
        -- Generate a response and send it. genEncryptionResponse can't directly
        -- create a Server.EncryptionResponse because that would require
        -- cyclical imports between Packet.Server and Tech.Encrypt
        sendPacket Server.encryptionResponse =<< uncurry (flip Server.EncryptionResponse) <$> genEncryptionResponse pubKey vt sharedSecret
        -- Only start encrypting *after* the encryption response is sent
        beginEncrypting sharedSecret
  }

instance PiskellReaction Client.LoginSuccess where {piskellReaction = piskellHandleLoginSuccess}
piskellHandleLoginSuccess :: PiskellPacketHandler Client.LoginSuccess
piskellHandleLoginSuccess = PiskellPacketHandler
  {reactToPacket = \(Client.LoginSuccess _uuid _username) -> do
    logg "Login Successful. Switching to `Play` mode."
    modify (\x -> x {currentState = Playing})
  }

instance PiskellReaction Client.SetCompression where {piskellReaction = piskellHandleSetCompression}
piskellHandleSetCompression :: PiskellPacketHandler Client.SetCompression
piskellHandleSetCompression = PiskellPacketHandler
  {reactToPacket = \(Client.SetCompression thresh) -> do
    logg $ "Compression threshold set to " <> T.pack (show thresh)
    beginCompression thresh
  }
    
  -- get Join Game
  -- get Plugin Message with Brand
  -- get Difficulty
  -- get Spawn position
  -- get plaer abilities

instance PiskellReaction Client.PlayerAbilities where {piskellReaction = piskellHandlePlayerAbilities}
piskellHandlePlayerAbilities :: PiskellPacketHandler Client.PlayerAbilities
piskellHandlePlayerAbilities = PiskellPacketHandler
  {reactToPacket = \(Client.PlayerAbilities _ _ _) -> do
    sendPacket Server.pluginMessage (Server.PluginMessage "MC|Brand" . runPutS . serialize @ProtocolString $ "Piskell Client")
    sendPacket Server.clientSettings (Server.ClientSettings "en_US" 8 {-Chunks Render distance-} 0 {-Chat enabled-} True {-Chat Colors enabled-} 0x7f {-All skin componnents enabled-} 1 {-Main hand: Right-})
  }

instance PiskellReaction Client.PlayerPositionAndLook where {piskellReaction = piskellHandlePlayerPositionAndLook}
piskellHandlePlayerPositionAndLook :: PiskellPacketHandler Client.PlayerPositionAndLook
piskellHandlePlayerPositionAndLook = PiskellPacketHandler
  {reactToPacket = \(Client.PlayerPositionAndLook pos look _relFlags tpId) -> do
    logg $ "Player position and look: " <> T.pack (show pos)
    sendPacket Server.tpConfirm (Server.TPConfirm tpId)
    sendPacket Server.playerPositionAndLook (Server.PlayerPositionAndLook pos look True)
    sendPacket Server.clientStatus (Server.ClientStatus PerformRespawn)
  }

instance PiskellReaction Client.Disconnect where {piskellReaction = piskellHandleDisconnect}
piskellHandleDisconnect :: PiskellPacketHandler Client.Disconnect
piskellHandleDisconnect = PiskellPacketHandler
  {reactToPacket = \(Client.Disconnect (ProtocolString reason)) -> logg $ "Disconnect because: " <> T.pack reason
  }

packetLoop :: (Logs r,SendsPackets r,HasPiskell r,Networks r,PerformsIO r) => Eff r ()
packetLoop = do
  -- Block until a packet arrives, then deal with it
  getGenericPacket getParser >>= \case
    Nothing -> loge "Failed to parse incoming packet"
    Just (SuchThat (DescribedPacket desc (q :: qt))) -> reactToPacket (piskellReaction @qt) q
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

piLogin :: ParseSet
piLogin = HashSet.fromList 
  [ambiguate Client.loginSuccess
  ,ambiguate Client.encryptionRequest
  ,ambiguate Client.disconnect
  --,(packetId @Client.LegacyHandshake,ambiguate . Identity <$> deserialize @Client.LegacyHandshake)
  ]

class PiskellReaction p where {piskellReaction :: PiskellPacketHandler p}
-}
