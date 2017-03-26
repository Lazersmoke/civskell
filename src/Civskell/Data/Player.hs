{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Player where

import Control.Eff
import Control.Concurrent.STM
--import Control.Monad (forM_)
import Data.Word (Word8)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.SuchThat

import Civskell.Data.Types
import qualified Civskell.Packet.Clientbound as Client
import Civskell.Tech.Network
import Civskell.Data.Logging
import Civskell.Data.World

getPlayer :: HasPlayer r => Eff r PlayerData
getPlayer = usingPlayer readTVar

usingPlayer :: HasPlayer r => (TVar PlayerData -> STM a) -> Eff r a
usingPlayer = send . UsingPlayerData

overPlayer :: HasPlayer r => (PlayerData -> PlayerData) -> Eff r ()
overPlayer f = usingPlayer (flip modifyTVar f)

setUsername :: HasPlayer r => String -> Eff r ()
setUsername u = overPlayer $ \p -> p {clientUsername = u}

setBrand :: HasPlayer r => String -> Eff r ()
setBrand u = overPlayer $ \p -> p {clientBrand = u}

setUUID :: HasPlayer r => UUID -> Eff r ()
setUUID u = overPlayer $ \p -> p {clientUUID = u}

setHolding :: HasPlayer r => Short -> Eff r ()
setHolding u = overPlayer $ \p -> p {holdingSlot = u}

setMoveMode :: HasPlayer r => MoveMode -> Eff r ()
setMoveMode u = overPlayer $ \p -> p {moveMode = u}

setPlayerPos :: HasPlayer r => (Double,Double,Double) -> Eff r ()
setPlayerPos u = overPlayer $ \p -> p {playerPosition = u}

setPlayerViewAngle :: HasPlayer r => (Float,Float) -> Eff r ()
setPlayerViewAngle u = overPlayer $ \p -> p {viewAngle = u}

setPlayerState :: HasPlayer r => ServerState -> Eff r ()
setPlayerState u = overPlayer $ \p -> p {playerState = u}

-- Add a new tid to the que
{-# INLINE pendTeleport #-}
pendTeleport :: (SendsPackets r,Logs r,HasPlayer r) => (Double,Double,Double) -> (Float,Float) -> Word8 -> Eff r ()
pendTeleport xyz yp relFlag = do
  p <- usingPlayer $ \t -> do
    p <- readTVar t
    writeTVar t p {nextTid = 1 + nextTid p,teleportConfirmationQue = Set.insert (nextTid p) $ teleportConfirmationQue p}
    return p
  sendPacket (Client.PlayerPositionAndLook xyz yp relFlag (nextTid p))

-- Check if the tid is in the que. If it is, then clear and return true, else false
{-# INLINE clearTeleport #-}
clearTeleport :: HasPlayer r => VarInt -> Eff r Bool
clearTeleport tid = usingPlayer $ \t -> do
  p <- readTVar t 
  writeTVar t p {teleportConfirmationQue = Set.delete tid $ teleportConfirmationQue p}
  return (Set.member tid $ teleportConfirmationQue p)

-- We need the TQueue here to be the same one that is running in the sending thread
{-# INLINE initPlayer #-}
initPlayer :: (SendsPackets r, PerformsIO r, HasWorld r, Logs r) => TQueue (ForAny ClientPacket) -> Eff (Player ': r) a -> Eff r a
initPlayer pq e = do
  t <- send (newTVarIO playerData)
  runPlayer t e
  where
    playerData = PlayerData
      {teleportConfirmationQue = Set.empty
      ,nextTid = 0
      ,keepAliveQue = Set.empty
      ,nextKid = 0
      ,holdingSlot = 0
      ,playerPosition = (0,0,0)
      ,viewAngle = (0,0)
      ,gameMode = Survival
      ,playerInventory = Map.empty
      ,diggingBlocks = Map.empty
      ,moveMode = Walking
      ,playerState = Handshaking
      ,clientUsername = undefined
      ,clientBrand = undefined
      ,clientUUID = undefined
      ,playerId = undefined
      ,packetQueue = pq
      }

-- Set the player's gamemode
{-# INLINE setGamemode #-}
setGamemode :: (SendsPackets r,Logs r,HasPlayer r) => Gamemode -> Eff r ()
setGamemode g = do
  overPlayer $ \p -> p {gameMode = g}
  sendPacket (Client.ChangeGameState (ChangeGamemode g))
  case g of
    Survival -> sendPacket (Client.PlayerAbilities (AbilityFlags False False False False) 0 1)
    Creative -> sendPacket (Client.PlayerAbilities (AbilityFlags True False True True) 0 1)

{-# INLINE setInventorySlot #-}
setInventorySlot :: (SendsPackets r,Logs r, HasPlayer r) => Short -> Slot -> Eff r ()
setInventorySlot slotNum slot = do
  overPlayer $ \p -> p {playerInventory = Map.insert slotNum slot (playerInventory p)}
  sendPacket (Client.SetSlot 0 (civskellToClientSlot slotNum) slot)
 
--getPacket :: forall p r. (Logs r,HasPlayer r,Packet p) => Eff r (Maybe p)
--getPacket = send $ GetPacket (parsePacket @p)

--{-# INLINE flushInbox #-}
--flushInbox :: HasPlayer r => Eff r ()
--flushInbox = send FlushInbox

-- Hotbar: 0-8
-- Inventory: 9-35
-- Armor: Head to Toe: 36,37,38,39
-- Crafting: output: 40, inputs: tl: 41, tr: 42, bl: 43, br: 44
-- off hand: 45
{-# INLINE getInventorySlot #-}
getInventorySlot :: HasPlayer r => Short -> Eff r Slot
getInventorySlot slotNum = Map.findWithDefault EmptySlot slotNum . playerInventory <$> getPlayer

{-# INLINE registerPlayer #-}
registerPlayer :: HasPlayer r => Eff r PlayerId
registerPlayer = send RegisterPlayer

{-# INLINE logp #-}
logp :: (Logs r,HasPlayer r) => String -> Eff r ()
logp msg = flip logt msg =<< clientUsername <$> getPlayer

clientToCivskellSlot :: Short -> Short
clientToCivskellSlot s
  | s == (-1) = s
  | s <= 4 = s + 40
  | s <= 8 = s + 31
  | s <= 35 = s
  | s <= 44 = s - 36
  | s == 45 = s
  | otherwise = error "Bad Slot Number"

civskellToClientSlot :: Short -> Short
civskellToClientSlot s
  | s == (-1) = s
  | s <= 8 = s + 36
  | s <= 35 = s
  | s <= 39 = s - 31
  | s <= 44 = s - 40
  | s == 45 = s
  | otherwise = error "Bad Slot Number"

forkPlayer :: (HasPlayer q,PerformsIO r,SendsPackets r,HasWorld r,Logs r) => Eff (Player ': r) a -> Eff q (Eff r a)
forkPlayer = send . ForkPlayer

runPlayer :: (PerformsIO r, SendsPackets r, HasWorld r, Logs r) => TVar PlayerData -> Eff (Player ': r) a -> Eff r a
runPlayer _ (Pure x) = return x
runPlayer t (Eff u q) = case u of
  Inject (UsingPlayerData f) -> runPlayer t . runTCQ q =<< send (atomically $ f t)
  Inject RegisterPlayer -> runPlayer t . runTCQ q =<< newPlayer t
  Inject (ForkPlayer e) -> runPlayer t (runTCQ q (runPlayer t e))
  -- Not our turn
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runPlayer t (runTCQ q x)))

getAllTQ :: TQueue a -> STM [a]
getAllTQ q = tryReadTQueue q >>= \case
  Nothing -> return []
  Just a -> (a:) <$> getAllTQ q
