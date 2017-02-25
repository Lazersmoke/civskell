{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Networking where

import Control.Eff
import Data.Semigroup
import System.IO
import Text.Parsec
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Civskell.Data.Types
import Civskell.Tech.Encrypt
import Civskell.Tech.Parse
import Civskell.Tech.Serialization

type HasNetworking r = Member Networking r

data Networking a where
  SetCompressionLevel :: Maybe VarInt -> Networking ()
  AddCompression :: BS.ByteString -> Networking BS.ByteString
  RemoveCompression :: BS.ByteString -> Networking BS.ByteString
  SetupEncryption :: EncryptionCouplet -> Networking ()
  GetFromNetwork :: Int -> Networking BS.ByteString
  PutIntoNetwork :: BS.ByteString -> Networking ()
  IsPacketReady :: Networking Bool

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

runNetworking :: HasIO r => Maybe EncryptionCouplet -> Maybe VarInt -> Handle -> Eff (Networking ': r) a -> Eff r a
runNetworking _ _ _ (Pure x) = Pure x
runNetworking mEnc mThresh hdl (Eff u q) = case u of
  Weaken restOfU -> Eff restOfU (Singleton (runNetworking mEnc mThresh hdl . runTCQ q))
  Inject (GetFromNetwork len) -> do
    case mEnc of
      Nothing -> do
        dat <- send (BS.hGet hdl len)
        runNetworking mEnc mThresh hdl (runTCQ q dat)
      Just (c,e,d) -> do
        bs <- send (BS.hGet hdl len)
        let (bs',d') = cfb8Decrypt c d bs
        runNetworking (Just (c,e,d')) mThresh hdl (runTCQ q bs')
  Inject (PutIntoNetwork bs) -> do
    case mEnc of
      Nothing -> do
        send (BS.hPut hdl bs)
        runNetworking mEnc mThresh hdl (runTCQ q ())
      Just (c,e,d) -> do
        let (bs',e') = cfb8Encrypt c e bs
        send (BS.hPut hdl bs')
        runNetworking (Just (c,e',d)) mThresh hdl (runTCQ q ())
  Inject (SetCompressionLevel thresh) -> runNetworking mEnc thresh hdl (runTCQ q ())
  Inject (SetupEncryption couplet) -> runNetworking (Just couplet) mThresh hdl (runTCQ q ())
  Inject (AddCompression bs) -> case mThresh of
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
  Inject (RemoveCompression bs) -> case mThresh of
    Nothing -> case parse parseUncompPkt "" bs of
      Left _ -> runNetworking mEnc mThresh hdl (runTCQ q bs)
      Right pktData -> runNetworking mEnc mThresh hdl (runTCQ q pktData)
    Just _ -> case parse parseCompPkt "" bs of
      -- TODO: fix this
      Left _ -> runNetworking mEnc mThresh hdl (runTCQ q bs)
      Right (dataLen,compressedData) -> if dataLen == 0x00
        then runNetworking mEnc mThresh hdl (runTCQ q compressedData)
        else runNetworking mEnc mThresh hdl (runTCQ q (LBS.toStrict . Z.decompress . LBS.fromStrict $ compressedData))
  Inject IsPacketReady -> runNetworking mEnc mThresh hdl . runTCQ q =<< send (hReady hdl)

