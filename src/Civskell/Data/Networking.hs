{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Networking where

import Control.Monad.Freer
import Control.Concurrent.STM
import Data.Bytes.Serial
import Data.SuchThat
import Data.Bytes.Put
import Data.Bytes.Get
import qualified Data.Text as T
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

sendPacket :: SendsPackets r => OutboundPacketDescriptor p -> p -> Eff r ()
sendPacket d p = send (SendPacket (ambiguate $ DescribedPacket d p))

iSolemnlySwearIHaveNoIdeaWhatImDoing :: SendsPackets r => BS.ByteString -> Eff r ()
iSolemnlySwearIHaveNoIdeaWhatImDoing = send . UnsafeSendBytes

beginEncrypting :: SendsPackets r => BS.ByteString -> Eff r ()
beginEncrypting = send . BeginEncrypting

beginCompression :: SendsPackets r => VarInt -> Eff r ()
beginCompression = send . BeginCompression

-- `Packeting`/`SendsPackets` is a high level interface that explicitly marks raw bytes as unsafe
runPacketing :: (Logs r,Networks r) => Eff (Packeting ': r) a -> Eff r a
runPacketing = handleRelay pure (flip handlePacketing)

handlePacketing :: (Logs r,Networks r) => Arr r v a -> Packeting v -> Eff r a
handlePacketing k = \case
  -- Unpack from the existentials to get the type information into a skolem, scoped tyvar
  SendPacket (SuchThat (DescribedPacket pktDesc pkt)) -> do
    let pktHandler = packetHandler pktDesc
    let writePacket = serializePacket pktHandler pkt
    -- Log its hex dump
    logLevel ClientboundPacket $ showPacket pktDesc pkt
    logLevel HexDump . T.pack $ indentedHex (runPutS writePacket)
    -- Send it
    rPut =<< addCompression (runPutS $ serialize (packetId pktHandler) *> writePacket)
    k ()
  -- "Unsafe" means it doesn't come from a legitimate packet (doesn't have packetId) and doesn't get compressed
  UnsafeSendBytes bytes -> rPut bytes *> k ()
  -- Passthrough for setting up encryption and compression
  BeginEncrypting ss -> setupEncryption ss *> k ()
  BeginCompression thresh -> setCompression thresh *> k ()

-- Used to fork a new thread that uses the same `TVar`s/`MVars` as this one
forkNetwork :: (Networks q,PerformsIO r) => Eff (Networking ': r) a -> Eff q (Eff r a)
forkNetwork = send . ForkNetwork

runNetworking :: PerformsIO r => TVar (Maybe EncryptionCouplet) -> TVar (Maybe VarInt) -> Handle -> Eff (Networking ': r) a -> Eff r a
runNetworking mEnc mThresh hdl = runNat (handleNetworking mEnc mThresh hdl)

handleNetworking :: TVar (Maybe EncryptionCouplet) -> TVar (Maybe VarInt) -> Handle -> Networking a -> IO a
handleNetworking mEnc mThresh hdl = \case
  ForkNetwork e -> pure (runNetworking mEnc mThresh hdl e)
  GetFromNetwork len -> do
    -- We don't want exclusive access here because we will block
    dat <- BS.hGet hdl len
    -- Decrypt the data or don't based on the encryption value
    atomically $ readTVar mEnc >>= \case
      Nothing -> return dat
      Just (c,e,d) -> do
        -- d' is the updated decrypting shift register
        let (bs',d') = cfb8Decrypt c d dat
        writeTVar mEnc (Just (c,e,d'))
        return bs'
  PutIntoNetwork bs -> do
    -- Encrypt the data or don't based on the encryption value
    enc <- atomically $ readTVar mEnc >>= \case
      Nothing -> return bs
      Just (c,e,d) -> do
        -- e' is the updated encrypting shift register
        let (bs',e') = cfb8Encrypt c e bs
        writeTVar mEnc (Just (c,e',d))
        return bs'
    BS.hPut hdl enc
  SetCompressionLevel thresh -> atomically $ readTVar mThresh >>= \case
    -- If there is no pre-existing threshold, set one
    Nothing -> writeTVar mThresh (Just thresh)
    -- If there is already compression, don't change it (only one setCompression is allowed)
    -- Should we throw an error instead of silently failing here?
    Just _ -> pure ()
  SetupEncryption ss -> atomically $ readTVar mThresh >>= \case
    -- If there is no encryption setup yet, set it up
    Nothing -> writeTVar mEnc (Just (makeEncrypter ss,ss,ss))
    -- If it is already encrypted, don't do anything
    Just _ -> pure ()
  AddCompression bs -> readTVarIO mThresh >>= \case
    -- Do not compress; annotate with length only
    Nothing -> pure (runPutS . withLength $ bs)
    -- Compress with the threshold `t`
    Just t -> if BS.length bs >= fromIntegral t
      -- Compress data and annotate to match
      then do
        -- Compress the actual data
        let compIdAndData = putByteString . LBS.toStrict . Z.compress . LBS.fromStrict $ bs
        -- Add the original size annotation
        let origSize = serialize $ ((fromIntegral (BS.length bs)) :: VarInt)
        let ann = withLength (runPutS $ origSize >> compIdAndData)
        pure (runPutS ann)
      -- Do not compress; annotate with length only
      -- 0x00 indicates it is not compressed
      else pure . runPutS . withLength . runPutS $ putWord8 0x00 *> putByteString bs
  RemoveCompression bs -> readTVarIO mThresh >>= \case
    -- Parse an uncompressed packet
    Nothing -> case runGetS parseUncompPkt bs of
      -- TODO: fix this completely ignoring a parse error
      Left _ -> putStrLn "Emergency Parse Error!!!" *> pure bs
      Right pktData -> pure pktData
    -- Parse a compressed packet (compressed packets have extra metadata)
    Just _ -> case parseOnly parseCompPkt bs of
      -- TODO: fix this completely ignoring a parse error
      Left _ -> pure bs
      Right (dataLen,compressedData) -> pure $ if dataLen == 0
        -- dataLen of 0 means that the packet is actually uncompressed
        then compressedData
        -- If it is indeed compressed, uncompress it
        else LBS.toStrict . Z.decompress . LBS.fromStrict $ compressedData
