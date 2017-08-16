{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Civskell.Data.World where

import Control.Concurrent.MVar (readMVar,modifyMVar,modifyMVar_,MVar)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Data.Functor.Identity
import Control.Monad.Freer
import Control.Monad (forM)
import Data.Bits
import Data.SuchThat
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map

import Civskell.Data.Types
import Civskell.Data.Protocol
import Civskell.Data.Player

-- All the information about a mineman world. Notably, players is a Map of PId's to TVars of player data, not actual player data
data WorldData = WorldData {chunks :: Map ChunkCoord ChunkSection, entities :: Map EntityId (Some Entity), players :: Map PlayerId (TVar PlayerData), nextEID :: EntityId, nextUUID :: UUID}

-- Having access to the World is an effect
type HasWorld r = Member World r

data World a where
  -- Commute a World effect out of a stack
  ForkWorld :: (Logs r,PerformsIO r) => Eff (World ': r) a -> World (Eff r a)
  -- TODO: redesign effect to slim down the effect interface to more general combinators, and bulk up the
  -- outside API interface to do what this is doing right now
  GetChunk :: ChunkCoord -> World ChunkSection
  RemoveBlock :: BlockCoord -> World ()
  SetBlock :: Some Block -> BlockCoord -> World ()
  SetChunk :: ChunkSection -> ChunkCoord -> World ()
  SetColumn :: [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> World ()
  WorldSTM :: STM a -> World a
  FreshEID :: World EntityId
  FreshUUID :: World UUID
  SetPlayer :: PlayerId -> PlayerData -> World ()
  GetPlayer :: PlayerId -> World PlayerData
  NewPlayer :: TVar PlayerData -> World PlayerId
  GetEntity :: EntityId -> World (Some Entity)
  DeleteEntity :: EntityId -> World ()
  SummonMob :: (Some Mob) -> World ()
  SummonObject :: (Some Object) -> World ()
  AllPlayers :: World [PlayerData]
  BroadcastPacket :: ForAny (DescribedPacket PacketSerializer) -> World ()


initWorld :: WorldData
initWorld = WorldData {chunks = Map.empty,entities = Map.empty,players = Map.empty,nextEID = 0,nextUUID = UUID (0,1)}

{-# INLINE getChunk #-}
getChunk :: HasWorld r => ChunkCoord -> Eff r ChunkSection
getChunk = send . GetChunk

{-# INLINE getBlock #-}
getBlock :: HasWorld r => BlockCoord -> Eff r (Some Block)
getBlock b = blockInChunk (blockToRelative b) <$> getChunk (blockToChunk b)

{-# INLINE setChunk #-}
setChunk :: HasWorld r => ChunkSection -> ChunkCoord -> Eff r ()
setChunk c' cc = send $ SetChunk c' cc

{-# INLINE setBlock #-}
setBlock :: (HasWorld r,Block b) => b -> BlockCoord -> Eff r ()
setBlock b' bc = send $ SetBlock (some b') bc

{-# INLINE setColumn #-}
setColumn :: HasWorld r => [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> Eff r ()
setColumn cs cxz mBio = send $ SetColumn cs cxz mBio

{-# INLINE removeBlock #-}
removeBlock :: HasWorld r => BlockCoord -> Eff r ()
removeBlock = send . RemoveBlock

{-# INLINE setPlayer #-}
setPlayer :: HasWorld r => PlayerId -> PlayerData -> Eff r ()
setPlayer i p = send $ SetPlayer i p

{-# INLINE lookupPlayer #-}
lookupPlayer :: HasWorld r => PlayerId -> Eff r PlayerData
lookupPlayer = send . GetPlayer

{-# INLINE modifyPlayer #-}
modifyPlayer :: HasWorld r => PlayerId -> (PlayerData -> PlayerData) -> Eff r ()
modifyPlayer i f = setPlayer i =<< f <$> lookupPlayer i

{-# INLINE freshEID #-}
freshEID :: HasWorld r => Eff r EntityId
freshEID = send FreshEID

{-# INLINE freshUUID #-}
freshUUID :: HasWorld r => Eff r UUID
freshUUID = send FreshUUID

{-# INLINE newPlayer #-}
newPlayer :: HasWorld r => TVar PlayerData -> Eff r PlayerId
newPlayer = send . NewPlayer

{-# INLINE allPlayers #-}
allPlayers :: HasWorld r => Eff r [PlayerData]
allPlayers = send AllPlayers

{-# INLINE broadcastPacket #-}
broadcastPacket :: HasWorld r => OutboundPacketDescriptor p -> p -> Eff r ()
broadcastPacket d p = send (BroadcastPacket (ambiguate $ DescribedPacket d p))

{-# INLINE getEntity #-}
getEntity :: (HasWorld r) => EntityId -> Eff r (Some Entity)
getEntity = send . GetEntity

{-# INLINE deleteEntity #-}
deleteEntity :: (HasWorld r) => EntityId -> Eff r ()
deleteEntity = send . DeleteEntity

{-# INLINE summonMob #-}
summonMob :: (HasWorld r, Mob m) => m -> Eff r ()
summonMob = send . SummonMob . ambiguate . Identity

{-# INLINE summonObject #-}
summonObject :: (HasWorld r, Object m) => m -> Eff r ()
summonObject = send . SummonObject . ambiguate . Identity

forkWorld :: (HasWorld q,Logs r,PerformsIO r) => Eff (World ': r) a -> Eff q (Eff r a)
forkWorld = send . ForkWorld

--data SpawnMob = SpawnMob EntityId String SomeMob (Double,Double,Double) Word8 Word8 Word8 Short Short Short -- Metadata NYI


