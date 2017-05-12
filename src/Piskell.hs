module Piskell where

import qualified Network as Net

import Civskell.Data.Types
import qualified Civskell.Packet.Serverbound as Server

runClient :: (Configured r,PerformsIO r) => Eff r ()
runClient = do
  -- Make a TQueue for logging
  logger <- send (newTQueueIO :: IO (TQueue String))
  -- Fork a thread to continuously log every log message
  _ <- send $ forkIO (loggingThread logger)
  -- Retire main thread to connection duty
  send . runM =<< (forkConfig . runLogger logger . login =<< send (Net.connectTo "localhost" (Net.PortNumber 25565))

-- Spawns a new thread to deal with each new client
login :: (Logs r,Configured r,PerformsIO r) => Handle -> Eff r ()
login hdl = do
  -- Log that we got a connection
  logg $ "Connecting to: localhost:25565"
  -- Don't line buffer the network
  send $ hSetBuffering hdl NoBuffering
  -- Make new networking TVars for this client
  netLock <- send $ newMVar ()
  mEnc <- send $ newTVarIO Nothing
  mThresh <- send $ newTVarIO Nothing
  -- Getting thread
  _ <- send . forkIO . runM =<< forkConfig =<< (forkLogger . runNetworking netLock mEnc mThresh handle =<< runPacketing packetLoop)
  -- Sending thread
  send . runM =<< forkConfig =<< (forkLogger . runNetworking netLock mEnc mThresh handle . runPacketing $ sendLoginPackets)

sendLoginPackets :: (Logs r,Configured r,PerformsIO r,SendsPackets r) => Eff r ()
sendLoginPackets = do
  sendPacket (Server.Handshake 316 "localhost" 25565 1)
  sendPacket (Server.LoginStart "piskell-client")
  -- This should be changed to use a) bytes Get/Put and b) Client.parseHandshakePacket
  (Client.EncryptionRequest sId pubKeyEnc vt) <- undefined :: SendsPackets r => Eff r Client.EncryptionRequest
  let sharedSecret = BS.pack [0x01,0x00,0x03,0x02,0x05,0x04,0x07,0x06,0x09,0x08,0x0B,0x0A,0x0D,0x0C,0x0F,0x0E]
  (ssResp,vtResp) <- genEncrytionResponse pubKeyEnc vt sharedSecret
  sendPacket (Server.EncryptionResponse ssResp vtResp)
  -- get Login Success
  -- get Join Game
  -- get Plugin Message with Brand
  -- get Difficulty
  -- get Spawn position
  -- get plaer abilities
  sendPacket (Server.PluginMessage "MC|Brand" . serialize . ProtocolString $ "Piskell Client")
  sendPacket (Server.ClientSettings (ProtocolString "en_US") 8 {-Chunks Render distance-} 0 {-Chat enabled-} True {-Chat Colors enabled-} 0x7f {-All skin componnents enabled-} 1 {-Main hand: Right-})
  -- get PlayerPositionAndLook
  let (pos,look,grounded) = ((0,0,0),(0,0),True)
  let idFromPlayerPositionAndLook = 0
  sendPacket (Server.TPConfirm idFromPlayerPositionAndLook)
  sendPacket (Server.PlayerPositionAndLook pos look grounded)
  sendPacket (Server.ClientStatus PerformRespawn)
  -- Other things from server

packetLoop :: (Logs r,Configured r,SendsPackets r) => Eff r ()
packetLoop = do
  -- Block until a packet arrives, then deal with it
  getGenericPacket getParser >>= \case
    Nothing -> loge "Failed to parse incoming packet"
    Just (SuchThat (SuchThat (Identity q))) -> do
      let op = onPacket q
      send . (>> pure ()) . forkIO =<< (runM <$>) . forkConfig =<< forkLogger =<< forkNetwork =<< forkWorld =<< (runPacketing <$> forkPlayer op)
  packetLoop
