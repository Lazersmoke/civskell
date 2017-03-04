{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Civskell.Data.World where

import Control.Concurrent.MVar (readMVar,modifyMVar,modifyMVar_,MVar)
import Control.Eff
import Control.Monad (forM)
import Data.Bits
import Data.List ((\\))
import Data.SuchThat
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map

import Civskell.Data.Types
import qualified Civskell.Packet.Clientbound as Client


initWorld :: WorldData
initWorld = WorldData {chunks = Map.empty,players = Map.empty, nextPlayerId = 0, broadcastLog = []}

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
setPlayer :: Member World r => PlayerId -> PlayerInfo -> Eff r ()
setPlayer i p = send $ SetPlayer i p

{-# INLINE getPlayer #-}
getPlayer :: Member World r => PlayerId -> Eff r PlayerInfo
getPlayer = send . GetPlayer

{-# INLINE modifyPlayer #-}
modifyPlayer :: Member World r => PlayerId -> (PlayerInfo -> PlayerInfo) -> Eff r ()
modifyPlayer i f = setPlayer i =<< f <$> getPlayer i

{-# INLINE newPlayer #-}
newPlayer :: Member World r => Eff r PlayerId
newPlayer = send NewPlayer

{-# INLINE allPlayers #-}
allPlayers :: Member World r => Eff r [PlayerInfo]
allPlayers = send AllPlayers

{-# INLINE forallPlayers #-}
forallPlayers :: Member World r => (PlayerInfo -> PlayerInfo) -> Eff r ()
forallPlayers = send . ForallPlayers

{-# INLINE inboxForPlayer #-}
inboxForPlayer :: Member World r => PlayerId -> Eff r [ForAny ClientPacket]
inboxForPlayer = send . InboxForPlayer

{-# INLINE broadcastPacket #-}
broadcastPacket :: (Member World r,Packet p, PacketSide p ~ 'Client,Serialize p) => p -> Eff r ()
broadcastPacket = send . BroadcastPacket . SuchThat . ClientPacket . SuchThatStar

runWorld :: (HasLogging r, HasIO r) => MVar WorldData -> Eff (World ': r) a -> Eff r a
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
  -- Create a new player with the default info, and return the new player's id. Also increment it for next time
  Inject NewPlayer -> (runWorld w' . runTCQ q =<<) . send . modifyMVar w' $ \w -> return (w {players = Map.insert (nextPlayerId w) defaultPlayerInfo (players w),nextPlayerId = succ (nextPlayerId w)},nextPlayerId w)
  -- Warning: throws error if player not found
  Inject (GetPlayer i) -> send (readMVar w') >>= runWorld w' . runTCQ q . flip (Map.!) i . players
  Inject (SetPlayer i p) -> do
    send $ modifyMVar_ w' $ \w -> return w {players = Map.insert i p (players w)}
    runWorld w' (runTCQ q ())
  Inject AllPlayers -> send (readMVar w') >>= runWorld w' . runTCQ q . Map.elems . players
  Inject (ForallPlayers f) -> do
    send $ modifyMVar_ w' $ \w -> return w {players = fmap f (players w)}
    runWorld w' (runTCQ q ())
  Inject (InboxForPlayer i) -> do
    (runWorld w' . runTCQ q =<<) . send . modifyMVar w' $ \w -> do
      let oldLog = broadcastLog w
      let sentMsgs = map snd $ filter (\(ids,_) -> not $ elem i ids) oldLog
      let newLog = filter (\(ids,_) -> not . null $ (Map.keys $ players w) \\ ids) . map (\(ids,pkt) -> (i:ids,pkt)) $ oldLog
      return (w {broadcastLog = newLog},sentMsgs)
  Inject (BroadcastPacket pkt) -> do
    send $ modifyMVar_ w' $ \w -> return w {broadcastLog = ([],pkt) : broadcastLog w}
    runWorld w' (runTCQ q ())
  Weaken u' -> Eff u' (Singleton (runWorld w' . runTCQ q))
