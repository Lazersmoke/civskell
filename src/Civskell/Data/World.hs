{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Civskell.Data.World where

import Control.Eff
import qualified Data.Map.Lazy as Map
import Control.Concurrent.MVar
import Civskell.Data.Types

type HasWorld = Member World

data World a where
  SetChunk :: ChunkSection -> ChunkCoord -> World ()
  GetChunk :: ChunkCoord -> World ChunkSection
  NewPlayer :: World PlayerId
  SetPlayer :: PlayerId -> PlayerInfo -> World ()
  GetPlayer :: PlayerId -> World PlayerInfo
  AllPlayers :: World [PlayerInfo]
  GetChatLog :: World [String]

getChunk :: Member World r => ChunkCoord -> Eff r ChunkSection
getChunk = send . GetChunk

getBlock :: Member World r => BlockCoord -> Eff r BlockState
getBlock b = blockInChunk (blockToRelative b) <$> getChunk (blockToChunk b)

setChunk :: Member World r => ChunkSection -> ChunkCoord -> Eff r ()
setChunk c cc = send $ SetChunk c cc

setBlock :: Member World r => BlockCoord -> BlockState -> Eff r ()
setBlock b b' = getChunk (blockToChunk b) >>= \(ChunkSection bs) -> setChunk (ChunkSection $ Map.insert (blockToRelative b) b' bs) (blockToChunk b)

removeBlock :: Member World r => BlockCoord -> Eff r ()
removeBlock bc = getChunk (blockToChunk bc) >>= \(ChunkSection bs) -> setChunk (ChunkSection $ Map.delete (blockToRelative bc) bs) (blockToChunk bc)

setPlayer :: Member World r => PlayerId -> PlayerInfo -> Eff r ()
setPlayer i p = send $ SetPlayer i p

getPlayer :: Member World r => PlayerId -> Eff r PlayerInfo
getPlayer = send . GetPlayer

newPlayer :: Member World r => Eff r PlayerId
newPlayer = send NewPlayer

allPlayers :: Member World r => Eff r [PlayerInfo]
allPlayers = send AllPlayers

getChatLog :: Member World r => Eff r [String]
getChatLog = send GetChatLog

--updateBreaks :: Member World r => Eff r ()
--updateBreaks = allPlayers >>= mapM_ $ \p -> do
  --forM (Map.toList $ diggingBlocks p) $ \(block,(BlockBreak stage)) -> if stage == 9
    --then removeBlock block >> return Nothing
    --else return $ Just (block,BlockBreak (stage + 1))

runWorld :: HasIO r => MVar WorldData -> Eff (World ': r) a -> Eff r a
runWorld _ (Pure x) = Pure x
runWorld w' (Eff u q) = case u of
  Inject (GetChunk chunk) -> send (takeMVar w') >>= \w -> case Map.lookup chunk (chunks w) of
    Just theChunk -> send (putMVar w' w) >> runWorld w' (runTCQ q theChunk)
    Nothing -> error $ "Tried to load non-existant chunk section: " ++ show chunk
  Inject (SetChunk c' cc) -> send (takeMVar w') >>= \w -> send (putMVar w' w {chunks = Map.insert cc c' (chunks w)}) >> runWorld w' (runTCQ q ())
  Inject NewPlayer -> send (takeMVar w') >>= \w -> send (putMVar w' w {nextPlayerId = succ (nextPlayerId w)}) >> runWorld w' (runTCQ q (nextPlayerId w))
  Inject (GetPlayer i) -> send (takeMVar w') >>= \w -> send (putMVar w' w) >> runWorld w' (runTCQ q (Map.findWithDefault defaultPlayerInfo i (players w)))
  Inject (SetPlayer i p) -> send (takeMVar w') >>= \w -> send (putMVar w' w {players = Map.insert i p (players w)}) >> runWorld w' (runTCQ q ())
  Inject AllPlayers -> send (takeMVar w') >>= \w -> send (putMVar w' w) >> runWorld w' (runTCQ q (Map.elems $ players w))
  Inject GetChatLog -> send (takeMVar w') >>= \w -> send (putMVar w' w) >> runWorld w' (runTCQ q (chatLog w))
  Weaken u' -> Eff u' (Singleton (runWorld w' . runTCQ q))
