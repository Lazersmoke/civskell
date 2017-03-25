{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
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

testInitWorld :: WorldData
testInitWorld = initWorld {chunks = Map.fromList [(ChunkCoord (cx,cy,cz),exampleChunk) | cx <- [-3..3], cz <- [-3..3], cy <- [0..7]]}
  where
    exampleChunk = ChunkSection $ Map.fromList [(Block (x,y,z),stone) | x <- [0..15], y <- [0..15], z <- [0..15]]
    stone = BlockState 1 0

{-# INLINE getChunk #-}
getChunk :: Member World r => ChunkCoord -> Eff r ChunkSection
getChunk = send . GetChunk

{-# INLINE getBlock #-}
getBlock :: Member World r => BlockCoord -> Eff r BlockState
getBlock b = blockInChunk (blockToRelative b) <$> getChunk (blockToChunk b)

{-# INLINE setChunk #-}
setChunk :: Member World r => ChunkSection -> ChunkCoord -> Eff r ()
setChunk c' cc = send $ SetChunk c' cc

{-# INLINE setBlock #-}
setBlock :: Member World r => BlockState -> BlockCoord -> Eff r ()
setBlock b' bc = send $ SetBlock b' bc

{-# INLINE setColumn #-}
setColumn :: Member World r => [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> Eff r ()
setColumn cs cxz mBio = send $ SetColumn cs cxz mBio

{-# INLINE colPacket #-}
colPacket :: HasWorld r => (Int,Int) -> Maybe BS.ByteString -> Eff r Client.ChunkData
colPacket (cx,cz) mbio = forM [0..15] (\cy -> getChunk (ChunkCoord (cx,cy,cz))) >>= \cs -> return (chunksToColumnPacket cs (cx,cz) mbio)

chunksToColumnPacket :: [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> Client.ChunkData
chunksToColumnPacket cs (cx,cz) mbio = Client.ChunkData (fromIntegral cx,fromIntegral cz) True (bitMask cs) (filter (not . isAirChunk) cs) mbio []
  where
    -- [Bool] -> VarInt basically
    bitMask as = foldr (\b i -> fromBool b .|. shiftL i 1) 0 (map (not . isAirChunk) as)
    fromBool True = 1
    fromBool False = 0
    isAirChunk (ChunkSection m) = Map.null m

{-# INLINE removeBlock #-}
removeBlock :: Member World r => BlockCoord -> Eff r ()
removeBlock = setBlock (BlockState 0 0)

{-# INLINE setPlayer #-}
setPlayer :: Member World r => PlayerId -> PlayerData -> Eff r ()
setPlayer i p = send $ SetPlayer i p

{-# INLINE lookupPlayer #-}
lookupPlayer :: Member World r => PlayerId -> Eff r PlayerData
lookupPlayer = send . GetPlayer

{-# INLINE modifyPlayer #-}
modifyPlayer :: Member World r => PlayerId -> (PlayerData -> PlayerData) -> Eff r ()
modifyPlayer i f = setPlayer i =<< f <$> lookupPlayer i

{-# INLINE freshEID #-}
freshEID :: Member World r => Eff r EntityId
freshEID = send FreshEID

{-# INLINE freshUUID #-}
freshUUID :: Member World r => Eff r UUID
freshUUID = send FreshUUID

{-# INLINE newPlayer #-}
newPlayer :: Member World r => TVar PlayerData -> Eff r PlayerId
newPlayer = send . NewPlayer

{-# INLINE allPlayers #-}
allPlayers :: Member World r => Eff r [PlayerData]
allPlayers = send AllPlayers

--{-# INLINE forallPlayers #-}
--forallPlayers :: Member World r => (PlayerInfo -> PlayerInfo) -> Eff r ()
--forallPlayers = send . ForallPlayers

--{-# INLINE inboxForPlayer #-}
--inboxForPlayer :: Member World r => PlayerId -> Eff r [ForAny ClientPacket]
--inboxForPlayer = send . InboxForPlayer

{-# INLINE broadcastPacket #-}
broadcastPacket :: (Member World r,Packet p, PacketSide p ~ 'Client,Serialize p) => p -> Eff r ()
broadcastPacket = send . BroadcastPacket . ambiguate . ClientPacket . ambiguate . Identity

{-# INLINE getEntity #-}
getEntity :: (Member World r) => EntityId -> Eff r (Some Entity)
getEntity = send . GetEntity

{-# INLINE deleteEntity #-}
deleteEntity :: (Member World r) => EntityId -> Eff r ()
deleteEntity = send . DeleteEntity

{-# INLINE summonMob #-}
summonMob :: (Member World r, Mob m) => m -> Eff r ()
summonMob = send . SummonMob . ambiguate . Identity

{-# INLINE summonObject #-}
summonObject :: (Member World r, Object m) => m -> Eff r ()
summonObject = send . SummonObject . ambiguate . Identity

--data SpawnMob = SpawnMob EntityId String SomeMob (Double,Double,Double) Word8 Word8 Word8 Short Short Short -- Metadata NYI

runWorld :: (Logs r, PerformsIO r) => MVar WorldData -> Eff (World ': r) a -> Eff r a
runWorld _ (Pure x) = Pure x
runWorld w' (Eff u q) = case u of
  Inject (GetChunk chunk) -> runWorld w' . runTCQ q . Map.findWithDefault (ChunkSection Map.empty) chunk . chunks =<< send (readMVar w')
  Inject (SetBlock b' bc) -> do
    send $ modifyMVar_ w' $ \w -> do
      let f = if b' == BlockState 0 0 then Map.delete (blockToRelative bc) else Map.insert (blockToRelative bc) b'
      return w {chunks = Map.alter (Just . maybe (ChunkSection $ f Map.empty) (\(ChunkSection c) -> ChunkSection $ f c)) (blockToChunk bc) (chunks w)}
    runWorld w' $ broadcastPacket (Client.BlockChange bc b')
    runWorld w' (runTCQ q ())
  Inject (SetChunk c' cc@(ChunkCoord (x,y,z))) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = Map.insert cc c' (chunks w)}
    runWorld w' $ broadcastPacket (Client.ChunkData (fromIntegral x,fromIntegral z) False (bit y) [c'] Nothing [])
    runWorld w' (runTCQ q ())
  Inject (SetColumn col' (cx,cz) mBio) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = fst $ foldl (\(m,i) c -> (Map.insert (ChunkCoord (cx,i,cz)) c m,i + 1)) (chunks w,0) col'}
    runWorld w' $ broadcastPacket $ chunksToColumnPacket col' (cx,cz) mBio
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
    (eid,uuid) <- send . modifyMVar w' $ \w -> do
      return (w {entities = Map.insert (nextEID w) (SuchThat m) (entities w),nextEID = succ (nextEID w),nextUUID = incUUID (nextUUID w)},(nextEID w,nextUUID w))
    runWorld w' $ broadcastPacket $ Client.SpawnMob eid uuid (SuchThat m) 0
    runWorld w' (runTCQ q ())
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  Inject (SummonObject (SuchThat m)) -> do
    (eid,uuid) <- send . modifyMVar w' $ \w -> do
      return (w {entities = Map.insert (nextEID w) (SuchThat m) (entities w),nextEID = succ (nextEID w),nextUUID = incUUID (nextUUID w)},(nextEID w,nextUUID w))
    runWorld w' $ broadcastPacket $ Client.SpawnObject eid uuid (SuchThat m)
    runWorld w' $ broadcastPacket $ Client.UpdateMetadata eid (map Just $ entityMeta (runIdentity m))
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
  Weaken u' -> Eff u' (Singleton (runWorld w' . runTCQ q))
