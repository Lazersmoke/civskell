{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Player where

import Control.Monad.Freer
import Control.Concurrent.STM
--import Control.Monad (forM_)
import Data.Word (Word8)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))
import Data.SuchThat

import Civskell.Data.Types
import Civskell.Data.Protocol
import Civskell.Item
import Civskell.Window
import Civskell.Tech.Network
import Civskell.Data.Logging
--import Civskell.Data.World

-- Having transactional access to a single Player's data is an effect
type HasPlayer r = Member Player r

data Player a where
  -- Do arbitrary STM given a TVar. TODO: can't they just do arbitrary STM in general if they happen upon a TVar other than the one we provide?
  UsingPlayerData :: (TVar PlayerData -> STM a) -> Player a
  -- Register a player with the World, and return their PId
  RegisterPlayer :: Player PlayerId
  -- Commute a Player effect out of a stack. This is like an "unjoin" in a way
  ForkPlayer :: (PerformsIO r, SendsPackets r, Logs r) => Eff (Player ': r) a -> Player (Eff r a)

-- Stores all the relevant information about a player
-- TODO: Extensibility?
data PlayerData = PlayerData
  {teleportConfirmationQue :: Set.Set VarInt
  ,nextTid :: VarInt
  ,keepAliveQue :: Set.Set VarInt
  ,nextKid :: VarInt
  ,nextWid :: WindowId
  -- Map of WindowId's to that window's metadata (including accessor effects)
  ,windows :: Map.Map WindowId (Some Window)
  ,failedTransactions :: Set.Set (WindowId,TransactionId)
  ,holdingSlot :: Short
  ,playerPosition :: (Double,Double,Double)
  ,viewAngle :: (Float,Float)
  ,gameMode :: Gamemode
  -- player inventory is window 0
  ,playerInventory :: Inventory
  ,diggingBlocks :: Map.Map BlockCoord BlockBreak
  ,moveMode :: MoveMode
  ,playerState :: ServerState
  ,clientUsername :: String
  ,clientBrand :: String
  ,clientUUID :: UUID
  ,playerId :: PlayerId
  ,packetQueue :: TQueue (ForAny (DescribedPacket PacketSerializer))
  }
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

forkPlayer :: (HasPlayer q,PerformsIO r,SendsPackets r,Logs r) => Eff (Player ': r) a -> Eff q (Eff r a)
forkPlayer = send . ForkPlayer

runPlayer :: (PerformsIO r, SendsPackets r, Logs r) => TVar PlayerData -> Eff (Player ': r) a -> Eff r a
runPlayer t = handleRelay pure (flip $ handlePlayer t)

handlePlayer :: (PerformsIO r, SendsPackets r, Logs r) => TVar PlayerData -> Arr r v a -> Player a -> Eff r a
handlePlayer k = \case
  UsingPlayerData f -> k =<< send (atomically $ f t)
  RegisterPlayer -> k =<< newPlayer t
  ForkPlayer e -> k (runPlayer t e)

getAllTQ :: TQueue a -> STM [a]
getAllTQ q = tryReadTQueue q >>= \case
  Nothing -> return []
  Just a -> (a:) <$> getAllTQ q
