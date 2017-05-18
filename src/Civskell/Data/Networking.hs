{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Networking where

import Control.Eff
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.SuchThat
import Data.Functor.Identity
import Data.Bytes.Serial
import Data.Bytes.Put
import System.IO
import Data.Attoparsec.ByteString
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Civskell.Data.Types
import Civskell.Data.Logging
import Civskell.Tech.Encrypt
import Civskell.Tech.Parse

{-# INLINE rGet #-}
rGet :: Networks n => Int -> Eff n BS.ByteString
rGet = send . GetFromNetwork

{-# INLINE rPut #-}
rPut :: Networks n => BS.ByteString -> Eff n ()
rPut = send . PutIntoNetwork

{-# INLINE setupEncryption #-}
setupEncryption :: Networks n => BS.ByteString -> Eff n ()
setupEncryption = send . SetupEncryption

{-# INLINE setCompression #-}
setCompression :: Networks n => VarInt -> Eff n ()
setCompression = send . SetCompressionLevel

{-# INLINE addCompression #-}
addCompression :: Networks n => BS.ByteString -> Eff n BS.ByteString
addCompression = send . AddCompression

{-# INLINE removeCompression #-}
removeCompression :: Networks n => BS.ByteString -> Eff n BS.ByteString
removeCompression = send . RemoveCompression

-- Send a Client Packet over the network
sendPacket :: (SendsPackets r,Serial p,Packet p,PacketSide p ~ 'Client) => p -> Eff r ()
sendPacket = sendAnyPacket . ambiguate . OutboundPacket . ambiguate . Identity

sendAnyPacket :: SendsPackets r => ForAny OutboundPacket -> Eff r ()
sendAnyPacket = send . SendPacket

iSolemnlySwearIHaveNoIdeaWhatImDoing :: SendsPackets r => BS.ByteString -> Eff r ()
iSolemnlySwearIHaveNoIdeaWhatImDoing = send . UnsafeSendBytes

beginEncrypting :: SendsPackets r => BS.ByteString -> Eff r ()
beginEncrypting = send . BeginEncrypting

beginCompression :: SendsPackets r => VarInt -> Eff r ()
beginCompression = send . BeginCompression

-- `Packeting`/`SendsPackets` is a high level interface that explicitly marks raw bytes as unsafe
runPacketing :: (Logs r,Networks r) => Eff (Packeting ': r) a -> Eff r a
runPacketing (Pure x) = Pure x
runPacketing (Eff u q) = case u of
  Weaken restOfU -> Eff restOfU (Singleton (runPacketing . runTCQ q))
  -- Unpack from the existentials to get the type information into a skolem, scoped tyvar
  Inject (SendPacket (SuchThat (OutboundPacket (SuchThat (Identity (s :: a)))))) -> do
    -- Log its hex dump
    logLevel ClientboundPacket $ showPacket s
    logLevel HexDump $ indentedHex (runPutS . serialize $ s)
    -- Send it
    rPut =<< addCompression (BS.append (runPutS . serialize $ packetId @a) . runPutS . serialize $ s)
    runPacketing (runTCQ q ())
  -- "Unsafe" means it doesn't come from a legitimate packet (doesn't have packetId) and doesn't get compressed
  Inject (UnsafeSendBytes bytes) -> rPut bytes >> runPacketing (runTCQ q ())
  -- Passthrough for setting up encryption and compression
  Inject (BeginEncrypting ss) -> setupEncryption ss >> runPacketing (runTCQ q ())
  Inject (BeginCompression thresh) -> setCompression thresh >> runPacketing (runTCQ q ())

-- Used to fork a new thread that uses the same `TVar`s/`MVars` as this one
forkNetwork :: (Networks q,PerformsIO r) => Eff (Networking ': r) a -> Eff q (Eff r a)
forkNetwork = send . ForkNetwork

runNetworking :: PerformsIO r => MVar () -> TVar (Maybe EncryptionCouplet) -> TVar (Maybe VarInt) -> Handle -> Eff (Networking ': r) a -> Eff r a
runNetworking _ _ _ _ (Pure x) = Pure x
runNetworking netLock mEnc mThresh hdl (Eff u q) = case u of
  Weaken restOfU -> Eff restOfU (Singleton (runNetworking netLock mEnc mThresh hdl . runTCQ q))
  Inject (ForkNetwork e) -> runNetworking netLock mEnc mThresh hdl (runTCQ q (runNetworking netLock mEnc mThresh hdl e))
  Inject (GetFromNetwork len) -> do
    -- We don't want exclusive access here because we will block
    dat <- send $ BS.hGet hdl len
    -- Decrypt the data or don't based on the encryption value
    clear <- send . atomically $ readTVar mEnc >>= \case
      Nothing -> return dat
      Just (c,e,d) -> do
        -- d' is the updated decrypting shift register
        let (bs',d') = cfb8Decrypt c d dat
        writeTVar mEnc (Just (c,e,d'))
        return bs'
    runNetworking netLock mEnc mThresh hdl (runTCQ q clear)
  Inject (PutIntoNetwork bs) -> do
    -- Encrypt the data or don't based on the encryption value
    enc <- send . atomically $ readTVar mEnc >>= \case
      Nothing -> return bs
      Just (c,e,d) -> do
        -- e' is the updated encrypting shift register
        let (bs',e') = cfb8Encrypt c e bs
        writeTVar mEnc (Just (c,e',d))
        return bs'
    -- Use the netLock to ensure we have exclusive access to the handle
    -- Aquire
    --send $ takeMVar netLock
    -- Write
    send $ BS.hPut hdl enc
    -- Release
    --send $ putMVar netLock ()
    runNetworking netLock mEnc mThresh hdl (runTCQ q ())
  Inject (SetCompressionLevel thresh) -> do
    send . atomically $ readTVar mThresh >>= \case
      -- If there is no pre-existing threshold, set one
      Nothing -> writeTVar mThresh (Just thresh)
      -- If there is already compression, don't change it (only one setCompression is allowed)
      -- Should we throw an error instead of silently failing here?
      Just _ -> pure ()
    runNetworking netLock mEnc mThresh hdl (runTCQ q ())
  Inject (SetupEncryption sharedSecret) -> do
    send . atomically $ readTVar mThresh >>= \case
      -- If there is no encryption setup yet, set it up
      Nothing -> writeTVar mEnc (Just (makeEncrypter sharedSecret,sharedSecret,sharedSecret))
      -- If it is already encrypted, don't do anything
      Just _ -> pure ()
    runNetworking netLock mEnc mThresh hdl (runTCQ q ())
  Inject (AddCompression bs) -> send (readTVarIO mThresh) >>= \case
    -- Do not compress; annotate with length only
    Nothing -> runNetworking netLock mEnc mThresh hdl (runTCQ q (runPutS . withLength . runPutS . serialize $ bs))
    -- Compress with the threshold `t`
    Just t -> if BS.length bs >= fromIntegral t
      -- Compress data and annotate to match
      then do
        -- Compress the actual data
        let compIdAndData = putByteString . LBS.toStrict . Z.compress . LBS.fromStrict $ bs
        -- Add the original size annotation
        let origSize = serialize $ ((fromIntegral (BS.length bs)) :: VarInt)
        let ann = withLength (runPutS $ origSize >> compIdAndData)
        runNetworking netLock mEnc mThresh hdl (runTCQ q (runPutS ann))
      -- Do not compress; annotate with length only
      else do
        -- 0x00 indicates it is not compressed
        let ann = withLength (runPutS $ putWord8 0x00 >> putByteString bs)
        runNetworking netLock mEnc mThresh hdl (runTCQ q (runPutS ann))
  Inject (RemoveCompression bs) -> send (readTVarIO mThresh) >>= \case
    -- Parse an uncompressed packet
    Nothing -> case parseOnly parseUncompPkt bs of
      -- TODO: fix this completely ignoring a parse error
      Left _ -> runNetworking netLock mEnc mThresh hdl (runTCQ q bs)
      Right pktData -> runNetworking netLock mEnc mThresh hdl (runTCQ q pktData)
    -- Parse a compressed packet (compressed packets have extra metadata)
    Just _ -> case parseOnly parseCompPkt bs of
      -- TODO: fix this completely ignoring a parse error
      Left _ -> runNetworking netLock mEnc mThresh hdl (runTCQ q bs)
      Right (dataLen,compressedData) -> if dataLen == 0
        -- dataLen of 0 means that the packet is actually uncompressed
        then runNetworking netLock mEnc mThresh hdl (runTCQ q compressedData)
        -- If it is indeed compressed, uncompress it
        else runNetworking netLock mEnc mThresh hdl (runTCQ q (LBS.toStrict . Z.decompress . LBS.fromStrict $ compressedData))
