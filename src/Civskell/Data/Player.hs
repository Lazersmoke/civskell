{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))

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

openNewWindow :: (SendsPackets r,HasPlayer r,Window w) => w -> ProtocolString -> Eff r WindowId
openNewWindow winType title = do
  wid <- usingPlayer $ \t -> do
    p <- readTVar t
    writeTVar t p {windows = Map.insert (nextWid p) (some winType) (windows p),nextWid = succ (nextWid p)}
    return (nextWid p)
  sendPacket (Client.openWindow 0x13) (Client.OpenWindow wid (some winType) title Nothing)
  return wid

openWindowWithItems :: (HasWorld r,SendsPackets r,HasPlayer r,Logs r,Window w) => w -> ProtocolString -> TVar Inventory -> Eff r WindowId
openWindowWithItems (ty :: tyT) name tI = do
  wid <- openNewWindow ty name
  playerInv <- Map.mapKeysMonotonic (+(27 - 9)) . playerInventory <$> getPlayer
  items <- Map.union playerInv <$> (send . WorldSTM $ readTVar tI)
  logp $ "Sending window " <> T.pack (show wid) <> " of type " <> T.pack (windowIdentifier @tyT) <> " with items " <> T.pack (show items)
  -- This is wrong because it doesn't pad between items
  sendPacket (Client.windowItems 0x14) (Client.WindowItems wid (ProtocolList $ Map.elems items) {-(slotCount @tyT)-})
  return wid

-- Add a new tid to the que
pendTeleport :: (SendsPackets r,Logs r,HasPlayer r) => (Double,Double,Double) -> (Float,Float) -> Word8 -> Eff r ()
pendTeleport xyz yp relFlag = do
  p <- usingPlayer $ \t -> do
    p <- readTVar t
    writeTVar t p {nextTid = 1 + nextTid p,teleportConfirmationQue = Set.insert (nextTid p) $ teleportConfirmationQue p}
    return p
  sendPacket (Client.playerPositionAndLook 0x2F) (Client.PlayerPositionAndLook xyz yp relFlag (nextTid p))

-- Check if the tid is in the que. If it is, then clear and return true, else false
clearTeleport :: HasPlayer r => VarInt -> Eff r Bool
clearTeleport tid = usingPlayer $ \t -> do
  p <- readTVar t 
  writeTVar t p {teleportConfirmationQue = Set.delete tid $ teleportConfirmationQue p}
  return (Set.member tid $ teleportConfirmationQue p)

-- Set the player's gamemode
setGamemode :: (SendsPackets r,Logs r,HasPlayer r) => Gamemode -> Eff r ()
setGamemode g = do
  overPlayer $ \p -> p {gameMode = g}
  sendPacket (Client.changeGameState 0x1E) (Client.ChangeGameState (ChangeGamemode g))
  case g of
    Survival -> sendPacket (Client.playerAbilities 0x2C) (Client.PlayerAbilities (AbilityFlags False False False False) 0 1)
    Creative -> sendPacket (Client.playerAbilities 0x2C) (Client.PlayerAbilities (AbilityFlags True False True True) 0 1)

-- Ideal version:
--
-- sendPacket (Client.ChangeGameState (ChangeGamemode g))
--

-- TODO: Semantic slot descriptors
setInventorySlot :: (SendsPackets r,Logs r, HasPlayer r) => Short -> Slot -> Eff r ()
setInventorySlot slotNum slotData = do
  overPlayer $ \p -> p {playerInventory = setSlot slotNum slotData (playerInventory p)}
  sendPacket (Client.setSlot 0x16) (Client.SetSlot 0 slotNum slotData)
 
--getPacket :: forall p r. (Logs r,HasPlayer r,Packet p) => Eff r (Maybe p)
--getPacket = send $ GetPacket (parsePacket @p)

--{-# INLINE flushInbox #-}
--flushInbox :: HasPlayer r => Eff r ()
--flushInbox = send FlushInbox

{-# INLINE getInventorySlot #-}
getInventorySlot :: (HasPlayer r) => Short -> Eff r Slot
getInventorySlot slotNum = renormalize . Map.lookup slotNum . playerInventory <$> getPlayer
  where
    renormalize (Just s) = s
    renormalize Nothing = Slot Nothing

{-# INLINE registerPlayer #-}
registerPlayer :: HasPlayer r => Eff r PlayerId
registerPlayer = send RegisterPlayer

{-# INLINE logp #-}
logp :: (Logs r,HasPlayer r) => Text -> Eff r ()
logp msg = flip logt msg =<< T.pack . clientUsername <$> getPlayer

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
