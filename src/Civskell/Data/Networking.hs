{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Networking where

import Control.Eff
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Data.Semigroup
import System.IO
import Data.Attoparsec.ByteString
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Civskell.Data.Types
import Civskell.Tech.Encrypt
import Civskell.Tech.Parse

{-# INLINE rGet #-}
rGet :: HasNetworking n => Int -> Eff n BS.ByteString
rGet = send . GetFromNetwork

{-# INLINE rPut #-}
rPut :: HasNetworking n => BS.ByteString -> Eff n ()
rPut = send . PutIntoNetwork

{-# INLINE setupEncryption #-}
setupEncryption :: HasNetworking n => EncryptionCouplet -> Eff n ()
setupEncryption = send . SetupEncryption

{-# INLINE setCompression #-}
setCompression :: HasNetworking n => Maybe VarInt -> Eff n ()
setCompression = send . SetCompressionLevel

{-# INLINE addCompression #-}
addCompression :: HasNetworking n => BS.ByteString -> Eff n BS.ByteString
addCompression = send . AddCompression

{-# INLINE removeCompression #-}
removeCompression :: HasNetworking n => BS.ByteString -> Eff n BS.ByteString
removeCompression = send . RemoveCompression

{-# INLINE isPacketReady #-}
isPacketReady :: HasNetworking n => Eff n Bool
isPacketReady = send IsPacketReady

runNetworking :: PerformsIO r => TVar (Maybe EncryptionCouplet) -> TVar (Maybe VarInt) -> Handle -> Eff (Networking ': r) a -> Eff r a
runNetworking _ _ _ (Pure x) = Pure x
runNetworking mEnc mThresh hdl (Eff u q) = case u of
  Weaken restOfU -> Eff restOfU (Singleton (runNetworking mEnc mThresh hdl . runTCQ q))
  Inject (GetFromNetwork len) -> do
    dat <- send $ BS.hGet hdl len
    clear <- send . atomically $ readTVar mEnc >>= \case
      Nothing -> return dat
      Just (c,e,d) -> do
        let (bs',d') = cfb8Decrypt c d dat
        writeTVar mEnc (Just (c,e,d'))
        return bs'
    runNetworking mEnc mThresh hdl (runTCQ q clear)
  Inject (PutIntoNetwork bs) -> do
    enc <- send . atomically $ readTVar mEnc >>= \case
      Nothing -> return bs
      Just (c,e,d) -> do
        let (bs',e') = cfb8Encrypt c e bs
        writeTVar mEnc (Just (c,e',d))
        return bs'
    send (BS.hPut hdl enc)
    runNetworking mEnc mThresh hdl (runTCQ q ())
  Inject (SetCompressionLevel thresh) -> send (atomically $ writeTVar mThresh thresh) >> runNetworking mEnc mThresh hdl (runTCQ q ())
  Inject (SetupEncryption couplet) -> send (atomically $ writeTVar mEnc (Just couplet)) >> runNetworking mEnc mThresh hdl (runTCQ q ())
  Inject (AddCompression bs) -> send (readTVarIO mThresh) >>= \case
    -- Do not compress; annotate with length only
    Nothing -> runNetworking mEnc mThresh hdl (runTCQ q (withLength bs))
    Just t -> if BS.length bs >= fromIntegral t
      -- Compress data and annotate to match
      then do
        let compIdAndData = LBS.toStrict . Z.compress . LBS.fromStrict $ bs
        let origSize = serialize $ ((fromIntegral (BS.length bs)) :: VarInt)
        let ann = withLength (origSize <> compIdAndData)
        runNetworking mEnc mThresh hdl (runTCQ q ann)
      -- Do not compress; annotate with length only
      else do
        let ann = withLength (BS.singleton 0x00 <> bs)
        runNetworking mEnc mThresh hdl (runTCQ q ann)
  Inject (RemoveCompression bs) -> send (readTVarIO mThresh) >>= \case
    Nothing -> case parseOnly parseUncompPkt bs of
      Left _ -> runNetworking mEnc mThresh hdl (runTCQ q bs)
      Right pktData -> runNetworking mEnc mThresh hdl (runTCQ q pktData)
    Just _ -> case parseOnly parseCompPkt bs of
      -- TODO: fix this completely ignoring a parse error
      Left _ -> runNetworking mEnc mThresh hdl (runTCQ q bs)
      Right (dataLen,compressedData) -> if dataLen == 0x00
        then runNetworking mEnc mThresh hdl (runTCQ q compressedData)
        else runNetworking mEnc mThresh hdl (runTCQ q (LBS.toStrict . Z.decompress . LBS.fromStrict $ compressedData))
  Inject IsPacketReady -> runNetworking mEnc mThresh hdl . runTCQ q =<< send isReady
    where
      isReady = do
        r <- not <$> hIsEOF hdl
        case r of
          True -> return True
          False -> threadDelay 5000 >> return False
