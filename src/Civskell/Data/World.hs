{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Civskell.Data.World where

import Control.Eff
import Data.Bits
import Data.List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Control.Concurrent.MVar
import Civskell.Data.Types
import Civskell.Data.Logging
import qualified Civskell.Packet.Clientbound as Client
import qualified Data.ByteString as BS

data WorldData = WorldData {chunks :: Map ChunkCoord ChunkSection, players :: Map PlayerId PlayerInfo, nextPlayerId :: PlayerId, broadcastLog :: [([PlayerId],Client.Packet)]}

initWorld :: WorldData
initWorld = WorldData {chunks = Map.empty,players = Map.empty, nextPlayerId = 0, broadcastLog = []}

type HasWorld = Member World

data World a where
  GetChunk :: ChunkCoord -> World ChunkSection
  SetBlock :: BlockState -> BlockCoord -> World ()
  SetChunk :: ChunkSection -> ChunkCoord -> World ()
  SetColumn :: [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> World ()
  NewPlayer :: World PlayerId
  SetPlayer :: PlayerId -> PlayerInfo -> World ()
  GetPlayer :: PlayerId -> World PlayerInfo
  AllPlayers :: World [PlayerInfo]
  InboxForPlayer :: PlayerId -> World [Client.Packet]
  BroadcastPacket :: Client.Packet -> World ()
  DumpBroadcast :: World ()

getChunk :: Member World r => ChunkCoord -> Eff r ChunkSection
getChunk = send . GetChunk

getBlock :: Member World r => BlockCoord -> Eff r BlockState
getBlock b = blockInChunk (blockToRelative b) <$> getChunk (blockToChunk b)

setChunk :: Member World r => ChunkSection -> ChunkCoord -> Eff r ()
setChunk c' cc = send $ SetChunk c' cc

setBlock :: Member World r => BlockState -> BlockCoord -> Eff r ()
setBlock b' bc = send $ SetBlock b' bc

setColumn :: Member World r => [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> Eff r ()
setColumn cs cxz mBio = send $ SetColumn cs cxz mBio

removeBlock :: Member World r => BlockCoord -> Eff r ()
removeBlock = setBlock (BlockState 0 0)

setPlayer :: Member World r => PlayerId -> PlayerInfo -> Eff r ()
setPlayer i p = send $ SetPlayer i p

getPlayer :: Member World r => PlayerId -> Eff r PlayerInfo
getPlayer = send . GetPlayer

newPlayer :: Member World r => Eff r PlayerId
newPlayer = send NewPlayer

allPlayers :: Member World r => Eff r [PlayerInfo]
allPlayers = send AllPlayers

inboxForPlayer :: Member World r => PlayerId -> Eff r [Client.Packet]
inboxForPlayer = send . InboxForPlayer

dumpBroadcast :: Member World r => Eff r ()
dumpBroadcast = send DumpBroadcast

broadcastPacket :: Member World r => Client.Packet -> Eff r ()
broadcastPacket = send . BroadcastPacket

--updateBreaks :: Member World r => Eff r ()
--updateBreaks = allPlayers >>= mapM_ $ \p -> do
  --forM (Map.toList $ diggingBlocks p) $ \(block,(BlockBreak stage)) -> if stage == 9
    --then removeBlock block >> return Nothing
    --else return $ Just (block,BlockBreak (stage + 1))

runWorld :: (HasLogging r, HasIO r) => MVar WorldData -> Eff (World ': r) a -> Eff r a
runWorld _ (Pure x) = Pure x
runWorld w' (Eff u q) = case u of
  Inject (GetChunk chunk) -> send (readMVar w') >>= \w -> case Map.lookup chunk (chunks w) of
    Just theChunk -> runWorld w' (runTCQ q theChunk)
    Nothing -> error $ "Tried to load non-existant chunk section: " ++ show chunk
  Inject (SetBlock b' bc) -> do
    send $ modifyMVar_ w' $ \w -> do
      let f = if b' == BlockState 0 0 then Map.delete (blockToRelative bc) else Map.insert (blockToRelative bc) b'
      return w {chunks = Map.adjust (\(ChunkSection c) -> ChunkSection $ f c) (blockToChunk bc) (chunks w)}
    logg $ "SetBlock " ++ show b' ++ " " ++ show bc
    actual <- runWorld w' $ getBlock bc
    logg $ "Actual: " ++ show actual
    runWorld w' $ broadcastPacket (Client.BlockChange bc b')
    runWorld w' (runTCQ q ())
  Inject (SetChunk c' cc@(ChunkCoord (x,y,z))) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = Map.insert cc c' (chunks w)}
    runWorld w' $ broadcastPacket (Client.ChunkData (fromIntegral x,fromIntegral z) False (bit y) [c'] Nothing [])
    runWorld w' (runTCQ q ())
  Inject (SetColumn col' (cx,cz) mBio) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = fst $ foldl (\(m,i) c -> (Map.insert (ChunkCoord (cx,i,cz)) c m,i + 1)) (chunks w,0) col'}
    runWorld w' $ broadcastPacket (Client.ChunkData (fromIntegral cx,fromIntegral cz) True (bitMask col') (filter (not . isAirChunk) col') mBio [])
    runWorld w' (runTCQ q ())
    where
      -- [Bool] -> VarInt basically
      bitMask as = foldl (\i b -> fromBool b .|. shiftL i 1) 0 (map isAirChunk as)
      fromBool True = 1
      fromBool False = 0
      isAirChunk (ChunkSection m) = Map.null m
  -- Create a new player with the default info, and return the new player's id. Also increment it for next time
  Inject NewPlayer -> (runWorld w' . runTCQ q =<<) . send . modifyMVar w' $ \w -> return (w {players = Map.insert (nextPlayerId w) defaultPlayerInfo (players w),nextPlayerId = succ (nextPlayerId w)},nextPlayerId w)
  -- Warning: throws error if player not found
  Inject (GetPlayer i) -> send (readMVar w') >>= runWorld w' . runTCQ q . flip (Map.!) i . players
  Inject (SetPlayer i p) -> do
    send $ modifyMVar_ w' $ \w -> return w {players = Map.insert i p (players w)}
    runWorld w' (runTCQ q ())
  Inject AllPlayers -> send (readMVar w') >>= runWorld w' . runTCQ q . Map.elems . players
  Inject DumpBroadcast -> do
    send $ modifyMVar_ w' $ \w -> return w {broadcastLog = []}
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
