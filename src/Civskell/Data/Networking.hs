{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
-- | Networking primitives.
module Civskell.Data.Networking where

import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import Control.Monad.Reader
import Control.Concurrent.STM

import Civskell.Data.Common
import Civskell.Data.Types
import Civskell.Data.Logging
import Civskell.Tech.Encrypt

-- | Parse an uncompressed packet to a @'BS.ByteString'@ holding that packet's data with the packet id.
parseUncompPkt :: MonadGet m => m BS.ByteString
parseUncompPkt = getByteString . fromIntegral =<< deserialize @VarInt

-- | Parse a compressed packet to a pair of its length and its data with the packet id.
parseCompPkt :: MonadGet m => m (VarInt,BS.ByteString)
parseCompPkt = do
  _ <- deserialize @VarInt
  dataLen <- deserialize @VarInt
  bs <- (getByteString . fromIntegral =<< remaining)
  pure (dataLen,bs)

-- | Read bytes from the network, decrypting if enabled
{-# INLINE rGet #-}
rGet :: Int -> Civskell BS.ByteString
rGet len = do
  hdl <- asks networkHandle
  -- We don't want exclusive access here because we will block
  dat <- lift $ BS.hGet hdl len
  mEnc <- asks networkEncryptionCouplet
  -- Decrypt the data or don't based on the encryption value
  lift . atomically $ readTVar mEnc >>= \case
    Nothing -> pure dat
    Just (c,e,d) -> do
      -- d' is the updated decrypting shift register
      let (bs',d') = cfb8Decrypt c d dat
      writeTVar mEnc (Just (c,e,d'))
      pure bs'
  
-- | Send bytes over the network, encrypting if enabled
{-# INLINE rPut #-}
rPut :: BS.ByteString -> Civskell ()
rPut bs = do
  mEnc <- asks networkEncryptionCouplet
  -- Encrypt the data or don't based on the encryption value
  enc <- lift . atomically $ readTVar mEnc >>= \case
    Nothing -> pure bs
    Just (c,e,d) -> do
      -- e' is the updated encrypting shift register
      let (bs',e') = cfb8Encrypt c e bs
      writeTVar mEnc (Just (c,e',d))
      pure bs'
  hdl <- asks networkHandle
  lift $ BS.hPut hdl enc
 
-- | Turn encryption on using the given Shared Secret
{-# INLINE setupEncryption #-}
setupEncryption :: BS.ByteString -> Civskell ()
setupEncryption ss = asks networkEncryptionCouplet >>= \net -> lift . atomically . writeTVar net $ Just (makeEncrypter ss,ss,ss)
  
-- | Turn compression on and set the threshold
{-# INLINE setCompression #-}
setCompression :: VarInt -> Civskell ()
setCompression thresh = asks networkCompressionThreshold >>= \net -> lift . atomically . writeTVar net $ Just thresh
 
-- | Add compression (if enabled) and packet metadata. Should be used after adding packetId, but before sending
{-# INLINE compressPacket #-}
compressPacket :: BS.ByteString -> Civskell BS.ByteString
compressPacket bs = asks networkCompressionThreshold >>= lift . readTVarIO >>= \case
  -- Do not compress; annotate with length only
  Nothing -> pure (runPutS . withLength $ bs)
  -- Compress with the threshold `t`
  Just t -> if BS.length bs >= fromIntegral t
    -- Compress data and annotate to match
    then let
      -- Compress the actual data
      compIdAndData = putByteString . LBS.toStrict . Z.compress . LBS.fromStrict $ bs
      -- Add the original size annotation
      origSize = serialize @VarInt . fromIntegral . BS.length $ bs
      in pure . runPutS $ withLength (runPutS $ origSize *> compIdAndData)
    -- Do not compress; annotate with length only
    -- 0x00 indicates it is not compressed
    else pure . runPutS . withLength . runPutS $ putWord8 0x00 *> putByteString bs
  
-- | Remove compression (if enabled) and strip packet metadata. Packets are left with packetId intact
{-# INLINE decompressPacket #-}
decompressPacket :: BS.ByteString -> Civskell (Either String BS.ByteString)
decompressPacket bs = asks networkCompressionThreshold >>= lift . readTVarIO >>= pure . \case
  -- Parse an uncompressed packet
  Nothing -> runGetS parseUncompPkt bs
  -- Parse a compressed packet (compressed packets have extra metadata)
  Just _ -> decompressPacket' <$> runGetS parseCompPkt bs
  where
    decompressPacket' (dataLen,compressedData) = if dataLen == 0
      -- dataLen of 0 means that the packet is actually uncompressed
      then compressedData
      -- If it is indeed compressed, uncompress it
      else LBS.toStrict . Z.decompress . LBS.fromStrict $ compressedData


-- | Send a packet for any packet state
sendPacket :: OutboundPacketDescriptor p -> p -> Civskell ()
sendPacket pktDesc pkt = do
  let pktHandler = packetHandler pktDesc
  let writePacket = serializePacket pktHandler pkt
  -- Log its hex dump
  logLevel ClientboundPacket $ showPacket pktDesc pkt
  logLevel HexDump . T.pack $ indentedHex (runPutS writePacket)
  -- Send it
  rPut =<< compressPacket (runPutS $ serialize (packetId pktHandler) *> writePacket)
