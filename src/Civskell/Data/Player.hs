{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Player where

import Control.Eff
import Control.Monad (forM_)
import Data.Word (Word8)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.SuchThat

import Civskell.Data.Types
import qualified Civskell.Packet.Clientbound as Client
import Civskell.Tech.Network
import Civskell.Data.World
import Civskell.Data.Logging

{-# INLINE getBrand #-}
getBrand :: HasPlayer r => Eff r String
getBrand = send PlayerBrand

{-# INLINE setBrand #-}
setBrand :: HasPlayer r => String -> Eff r ()
setBrand = send . SetPlayerBrand

{-# INLINE getUsername #-}
getUsername :: HasPlayer r => Eff r String
getUsername = send PlayerName

{-# INLINE setUsername #-}
setUsername :: HasPlayer r => String -> Eff r ()
setUsername = send . SetPlayerName

{-# INLINE getUUID #-}
getUUID :: HasPlayer r => Eff r String
getUUID = send PlayerUUID

{-# INLINE setUUID #-}
setUUID :: HasPlayer r => String -> Eff r ()
setUUID = send . SetPlayerUUID

{-# INLINE getHolding #-}
getHolding :: HasPlayer r => Eff r Short
getHolding = send PlayerHolding

{-# INLINE setHolding #-}
setHolding :: HasPlayer r => Short -> Eff r ()
setHolding = send . SetPlayerHolding

{-# INLINE pendTeleport #-}
pendTeleport :: HasPlayer r => (Double,Double,Double) -> (Float,Float) -> Word8 -> Eff r ()
pendTeleport xyz yp r = send (PlayerAddTP xyz yp r)

{-# INLINE clearTeleport #-}
clearTeleport :: HasPlayer r => VarInt -> Eff r Bool
clearTeleport = send . PlayerClearTP

{-# INLINE initPlayer #-}
initPlayer :: (HasWorld r, HasNetworking r, HasLogging r) => Eff (Player ': r) a -> Eff r a
initPlayer = (newPlayer >>=) . flip runPlayer

{-# INLINE getGamemode #-}
getGamemode :: HasPlayer r => Eff r Gamemode
getGamemode = send PlayerGamemode

{-# INLINE setGamemode #-}
setGamemode :: HasPlayer r => Gamemode -> Eff r ()
setGamemode = send . SetPlayerGamemode

{-# INLINE setPlayerPos #-}
setPlayerPos :: HasPlayer r => (Double,Double,Double) -> Eff r ()
setPlayerPos = send . SetPlayerPosition

{-# INLINE getPlayerPos #-}
getPlayerPos :: HasPlayer r => Eff r (Double,Double,Double)
getPlayerPos = send GetPlayerPosition

{-# INLINE setPlayerViewAngle #-}
setPlayerViewAngle :: HasPlayer r => (Float,Float) -> Eff r ()
setPlayerViewAngle = send . SetPlayerViewAngle

{-# INLINE getPlayerViewAngle #-}
getPlayerViewAngle :: HasPlayer r => Eff r (Float,Float)
getPlayerViewAngle = send GetPlayerViewAngle

{-# INLINE setInventorySlot #-}
setInventorySlot :: HasPlayer r => Short -> Slot -> Eff r ()
setInventorySlot a b = send $ SetPlayerSlot a b

{-# INLINE getMoveMode #-}
getMoveMode :: HasPlayer r => Eff r MoveMode
getMoveMode = send GetMoveMode

{-# INLINE setMoveMode #-}
setMoveMode :: HasPlayer r => MoveMode -> Eff r ()
setMoveMode = send . SetMoveMode

{-# INLINE flushInbox #-}
flushInbox :: HasPlayer r => Eff r ()
flushInbox = send FlushInbox

-- Hotbar: 0-8
-- Inventory: 9-35
-- Armor: Head to Toe: 36,37,38,39
-- Crafting: output: 40, inputs: tl: 41, tr: 42, bl: 43, br: 44
-- off hand: 45
{-# INLINE getInventorySlot #-}
getInventorySlot :: HasPlayer r => Short -> Eff r Slot
getInventorySlot = send . GetPlayerSlot

{-# INLINE logp #-}
logp :: HasPlayer r => String -> Eff r ()
logp = send . LogPlayerName

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

-- NOTE: always be atomic wrt [get,set,modify]Player
runPlayer :: (HasNetworking r, HasWorld r, HasLogging r) => PlayerId -> Eff (Player ': r) a -> Eff r a
runPlayer _ (Pure x) = return x
runPlayer i (Eff u q) = case u of
  -- Get the player's client brand
  Inject PlayerBrand -> runPlayer i . runTCQ q . clientBrand =<< getPlayer i
  -- Set the player's client brand
  Inject (SetPlayerBrand b) -> modifyPlayer i (\p -> p {clientBrand = b}) >> runPlayer i (runTCQ q ())
  -- Get the player's name
  Inject PlayerName -> runPlayer i . runTCQ q . clientUsername =<< getPlayer i
  -- Set the player's name
  Inject (SetPlayerName b) -> modifyPlayer i (\p -> p {clientUsername = b}) >> runPlayer i (runTCQ q ())
  -- Get the player's UUID
  Inject PlayerUUID -> runPlayer i . runTCQ q . clientUUID =<< getPlayer i
  -- Set the player's name
  Inject (SetPlayerUUID b) -> modifyPlayer i (\p -> p {clientUUID = b}) >> runPlayer i (runTCQ q ())
  -- Get the player's selected slot
  Inject PlayerGamemode -> runPlayer i . runTCQ q . gameMode =<< getPlayer i
  -- Set the player's selected slot
  Inject (SetPlayerGamemode g) -> do
    modifyPlayer i $ \p -> p {gameMode = g}
    sendPacket (Client.ChangeGameState (ChangeGamemode g))
    case g of
      Survival -> sendPacket (Client.PlayerAbilities (AbilityFlags False False False False) 0 1)
      Creative -> sendPacket (Client.PlayerAbilities (AbilityFlags True False True True) 0 1)
    runPlayer i (runTCQ q ())
  -- Get the player's selected slot
  Inject PlayerHolding -> runPlayer i . runTCQ q . holdingSlot =<< getPlayer i 
  -- Set the player's selected slot
  Inject (SetPlayerHolding s) -> modifyPlayer i (\p -> p {holdingSlot = s}) >> runPlayer i (runTCQ q ())
  -- Add a new tid to the que
  Inject (PlayerAddTP xyz yp relFlag) -> do
    p <- getPlayer i
    setPlayer i p {nextTid = 1 + nextTid p,teleportConfirmationQue = Set.insert (nextTid p) $ teleportConfirmationQue p}
    sendPacket (Client.PlayerPositionAndLook xyz yp relFlag (nextTid p))
    runPlayer i (runTCQ q ())
  -- Check if the tid is in the que. If it is, then clear and return true, else false
  Inject (PlayerClearTP tid) -> do
    p <- getPlayer i
    setPlayer i p {teleportConfirmationQue = Set.delete tid $ teleportConfirmationQue p}
    runPlayer i (runTCQ q $ Set.member tid $ teleportConfirmationQue p)
  Inject GetPlayerPosition -> runPlayer i . runTCQ q . playerPosition =<< getPlayer i
  Inject (SetPlayerPosition xyz) -> modifyPlayer i (\p -> p {playerPosition = xyz}) >> runPlayer i (runTCQ q ())
  Inject GetPlayerViewAngle -> runPlayer i . runTCQ q . viewAngle =<< getPlayer i
  Inject (SetPlayerViewAngle yp) -> modifyPlayer i (\p -> p {viewAngle = yp}) >> runPlayer i (runTCQ q ())
  Inject (GetPlayerSlot slotNum) -> runPlayer i . runTCQ q . Map.findWithDefault EmptySlot slotNum . playerInventory =<< getPlayer i
  Inject (SetPlayerSlot slotNum slot) -> do
    modifyPlayer i $ \p -> p {playerInventory = Map.insert slotNum slot (playerInventory p)}
    sendPacket (Client.SetSlot 0 (civskellToClientSlot slotNum) slot)
    runPlayer i (runTCQ q ())
  Inject (StartBreaking block) -> do
    modifyPlayer i $ \p -> p {diggingBlocks = Map.insert block (InProgress 0) (diggingBlocks p)} 
    runPlayer i (runTCQ q ())
  Inject (StopBreaking block) -> modifyPlayer i (\p -> p {diggingBlocks = Map.delete block (diggingBlocks p)}) >> runPlayer i (runTCQ q ())
  Inject GetMoveMode -> runPlayer i . runTCQ q . moveMode =<< getPlayer i
  Inject (SetMoveMode mode) -> modifyPlayer i (\p -> p {moveMode = mode}) >> runPlayer i (runTCQ q ())
  Inject FlushInbox -> do
    pkts <- inboxForPlayer i
    forM_ pkts (\(SuchThat p) -> sendClientPacket p)
    runPlayer i (runTCQ q ())
  Inject (LogPlayerName msg) -> (>> runPlayer i (runTCQ q ())) . flip logt msg . clientUsername =<< getPlayer i
  -- Not our turn
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runPlayer i (runTCQ q x)))

