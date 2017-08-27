{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Networking where

import Control.Monad.Freer
import Data.SuchThat
import qualified Data.ByteString as BS

--import Civskell.Tech.Encrypt
--import Civskell.Tech.Parse

import Civskell.Data.Common

{-# INLINE rGet #-}
rGet :: Member Networking n => Int -> Eff n BS.ByteString
rGet = send . GetFromNetwork

{-# INLINE rPut #-}
rPut :: Member Networking n => BS.ByteString -> Eff n ()
rPut = send . PutIntoNetwork

{-# INLINE setupEncryption #-}
setupEncryption :: Member Networking n => BS.ByteString -> Eff n ()
setupEncryption = send . SetupEncryption

{-# INLINE setCompression #-}
setCompression :: Member Networking n => VarInt -> Eff n ()
setCompression = send . SetCompressionLevel

{-# INLINE addCompression #-}
addCompression :: Member Networking n => BS.ByteString -> Eff n BS.ByteString
addCompression = send . AddCompression

{-# INLINE removeCompression #-}
removeCompression :: Member Networking n => BS.ByteString -> Eff n BS.ByteString
removeCompression = send . RemoveCompression

sendPacket :: Member Packeting r => OutboundPacketDescriptor p -> p -> Eff r ()
sendPacket d p = send (SendPacket (ambiguate $ DescribedPacket d p))

iSolemnlySwearIHaveNoIdeaWhatImDoing :: Member Packeting r => BS.ByteString -> Eff r ()
iSolemnlySwearIHaveNoIdeaWhatImDoing = send . UnsafeSendBytes

beginEncrypting :: Member Packeting r => BS.ByteString -> Eff r ()
beginEncrypting = send . BeginEncrypting

beginCompression :: Member Packeting r => VarInt -> Eff r ()
beginCompression = send . BeginCompression

-- Having access to low-level networking operations is an effect
data Networking a where
  -- Commute a Networking effect off a stack
  --ForkNetwork :: PerformsIO r => Eff (Networking ': r) a -> Networking (Eff r a)
  -- Turn compression on and set the threshold
  SetCompressionLevel :: VarInt -> Networking ()
  -- Add compression (if enabled) and packet metadata. Should be used after adding packetId, but before sending
  AddCompression :: BS.ByteString -> Networking BS.ByteString
  -- Remove compression (if enabled) and strip packet metadata. Packets are left with packetId intact
  RemoveCompression :: BS.ByteString -> Networking BS.ByteString
  -- Turn encryption on using the given Shared Secret
  SetupEncryption :: BS.ByteString -> Networking ()
  -- Send bytes over the network, encrypting if enabled
  PutIntoNetwork :: BS.ByteString -> Networking ()
  -- Read bytes from the network, decrypting if enabled
  GetFromNetwork :: Int -> Networking BS.ByteString

-- Having access to a high-level networking interface is an effect
data Packeting a where
  -- Send a packet for any packet state
  SendPacket :: ForAny (DescribedPacket PacketSerializer) -> Packeting ()
  -- Send raw bytes over the network (used in LegacyHandshakePong)
  UnsafeSendBytes :: BS.ByteString -> Packeting ()
  -- Use the given shared secret to enable encryption
  BeginEncrypting :: BS.ByteString -> Packeting ()
  -- Use the given threshold to enable compression
  BeginCompression :: VarInt -> Packeting ()

-- Used to fork a new thread that uses the same `TVar`s/`MVars` as this one
--forkNetwork :: (Member Networking q,PerformsIO r) => Eff (Networking ': r) a -> Eff q (Eff r a)
--forkNetwork = send . ForkNetwork

