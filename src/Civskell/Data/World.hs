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
import Control.Eff
import Control.Monad (forM)
import Data.Bits
import Data.SuchThat
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map

import Civskell.Data.Types
import qualified Civskell.Packet.Clientbound as Client


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

{-# INLINE colPacket #-}
colPacket :: HasWorld r => (Int,Int) -> Maybe BS.ByteString -> Eff r Client.ChunkData
colPacket (cx,cz) mbio = forM [0..15] (\cy -> getChunk (ChunkCoord (cx,cy,cz))) >>= \cs -> return (chunksToColumnPacket cs (cx,cz) mbio)

chunksToColumnPacket :: [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> Client.ChunkData
chunksToColumnPacket cs (cx,cz) mbio = Client.ChunkData (fromIntegral cx,fromIntegral cz) True (bitMask cs) (filter (not . isAirChunk) cs) mbio (ProtocolList [])
  where
    -- [Bool] -> VarInt basically
    bitMask as = foldr (\b i -> fromBool b .|. shiftL i 1) 0 (map (not . isAirChunk) as)
    fromBool True = 1
    fromBool False = 0
    isAirChunk (ChunkSection m) = Map.null m

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

runWorld :: (Logs r, PerformsIO r) => MVar WorldData -> Eff (World ': r) a -> Eff r a
runWorld _ (Pure x) = Pure x
runWorld w' (Eff u q) = case u of
  Inject (ForkWorld e) -> runWorld w' (runTCQ q (runWorld w' e))
  Inject (GetChunk chunk) -> runWorld w' . runTCQ q . Map.findWithDefault (ChunkSection Map.empty) chunk . chunks =<< send (readMVar w')
  Inject (RemoveBlock bc) -> do
    send $ modifyMVar_ w' $ \w -> do
      let f = Map.delete (blockToRelative bc)
      return w {chunks = Map.alter (Just . maybe (ChunkSection $ f Map.empty) (\(ChunkSection c) -> ChunkSection $ f c)) (blockToChunk bc) (chunks w)}
    runWorld w' $ broadcastPacket Client.blockChange (Client.BlockChange bc (some Air))
    runWorld w' (runTCQ q ())
  Inject (SetBlock b' bc) -> do
    send $ modifyMVar_ w' $ \w -> do
      let f = Map.insert (blockToRelative bc) b'
      return w {chunks = Map.alter (Just . maybe (ChunkSection $ f Map.empty) (\(ChunkSection c) -> ChunkSection $ f c)) (blockToChunk bc) (chunks w)}
    runWorld w' $ broadcastPacket Client.blockChange (Client.BlockChange bc b')
    runWorld w' (runTCQ q ())
  Inject (SetChunk c' cc@(ChunkCoord (x,y,z))) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = Map.insert cc c' (chunks w)}
    runWorld w' $ broadcastPacket Client.chunkData (Client.ChunkData (fromIntegral x,fromIntegral z) False (bit y) [c'] Nothing (ProtocolList []))
    runWorld w' (runTCQ q ())
  Inject (SetColumn col' (cx,cz) mBio) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = fst $ foldl (\(m,i) c -> (Map.insert (ChunkCoord (cx,i,cz)) c m,i + 1)) (chunks w,0) col'}
    runWorld w' $ broadcastPacket Client.chunkData $ chunksToColumnPacket col' (cx,cz) mBio
    runWorld w' (runTCQ q ())
  Inject FreshEID -> (>>= runWorld w' . runTCQ q) . send . modifyMVar w' $ \w -> return (w {nextEID = succ $ nextEID w},nextEID w)
  Inject FreshUUID -> (>>= runWorld w' . runTCQ q) . send . modifyMVar w' $ \w -> return (w {nextUUID = incUUID $ nextUUID w},nextUUID w)
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  -- Warning: throws error if player not found
  Inject (GetPlayer i) -> runWorld w' . runTCQ q =<< send . readTVarIO . flip (Map.!) i . players =<< send (readMVar w')
  -- Warning: throws error if player not found
  Inject (SetPlayer i p) -> do
    send . atomically . flip writeTVar p . flip (Map.!) i . players =<< send (readMVar w')
    runWorld w' (runTCQ q ())
  Inject (NewPlayer t) -> do
    pid <- runWorld w' freshEID
    send $ modifyMVar_ w' $ \w -> return w {players = Map.insert pid t $ players w}
    runWorld w' (runTCQ q pid)
  Inject (GetEntity e) -> send (readMVar w') >>= runWorld w' . runTCQ q . flip (Map.!) e . entities
  Inject (DeleteEntity e) -> send (modifyMVar_ w' $ \w -> return w {entities = Map.delete e . entities $ w}) >> runWorld w' (runTCQ q ())
  -- Pattern match on SuchThat to reinfer the constrain `Mob` into the contraint `Entity` for storage in the entities map
  Inject (SummonMob (SuchThat m)) -> do
    (eid,uuid) <- send . modifyMVar w' $ \w ->
      return (w {entities = Map.insert (nextEID w) (SuchThat m) (entities w),nextEID = succ (nextEID w),nextUUID = incUUID (nextUUID w)},(nextEID w,nextUUID w))
    runWorld w' $ broadcastPacket Client.spawnMob $ Client.makeSpawnMob eid uuid 0 (SuchThat m)
    runWorld w' (runTCQ q ())
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  Inject (SummonObject (SuchThat m)) -> do
    (eid,uuid) <- send . modifyMVar w' $ \w ->
      return (w {entities = Map.insert (nextEID w) (SuchThat m) (entities w),nextEID = succ (nextEID w),nextUUID = incUUID (nextUUID w)},(nextEID w,nextUUID w))
    runWorld w' $ broadcastPacket Client.spawnObject $ Client.SpawnObject eid uuid (SuchThat m)
    runWorld w' $ broadcastPacket Client.updateMetadata $ Client.UpdateMetadata eid (EntityPropertySet $ map Just $ entityMeta (runIdentity m))
    runWorld w' (runTCQ q ())
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  Inject AllPlayers -> runWorld w' . runTCQ q =<< send . atomically . traverse readTVar . Map.elems . players =<< send (readMVar w')
  --Inject (ForallPlayers f) -> do
    --send $ modifyMVar_ w' $ \w -> return w {players = fmap f (players w)}
    --runWorld w' (runTCQ q ())
  Inject (BroadcastPacket pkt) -> do
    send . atomically . mapM_ (\p -> readTVar p >>= \pd -> case playerState pd of {Playing -> flip writeTQueue pkt . packetQueue $ pd; _ -> pure ()}) . players =<< send (readMVar w')
    runWorld w' (runTCQ q ())
  Inject (WorldSTM stm) -> runWorld w' . runTCQ q =<< send (atomically stm)
  Weaken u' -> Eff u' (Singleton (runWorld w' . runTCQ q))
