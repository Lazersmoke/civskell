module Piskell where

import qualified Network as Net

import Civskell.Data.Types
import qualified Civskell.Packet.Serverbound as Server

runClient :: (Configured r,PerformsIO r) => Eff r ()
runClient = do
  -- Make a TQueue for logging
  logger <- send (newTQueueIO :: IO (TQueue String))
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
  
