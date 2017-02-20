{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Player where

import Control.Eff
import Control.Monad
import Civskell.Data.Types
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import qualified Civskell.Packet.Clientbound as Client
import Civskell.Tech.Network
import Civskell.Data.World
import Civskell.Data.Logging

type HasPlayer = Member Player

data Player a where
  -- Simple maybe state for brand
  PlayerBrand :: Player (Maybe String)
  SetPlayerBrand :: String -> Player ()
  -- Simple state for selected slot
  PlayerHolding :: Player Short
  SetPlayerHolding :: Short -> Player ()
  -- Simple state for Gamemode
  PlayerGamemode :: Player Gamemode
  SetPlayerGamemode :: Gamemode -> Player ()
  -- Teleport confirm que
  PlayerAddTP :: (Double,Double,Double) -> (Float,Float) -> Word8 -> Player ()
  PlayerClearTP :: VarInt -> Player Bool
  -- Keep Alive confirm que
  PlayerAddKeepAlive :: Player ()
  PlayerClearKeepAlive :: VarInt -> Player Bool
  -- Position and Look
  GetPlayerPosition :: Player (Double,Double,Double)
  SetPlayerPosition :: (Double,Double,Double) -> Player ()
  GetPlayerViewAngle :: Player (Float,Float)
  SetPlayerViewAngle :: (Float,Float) -> Player ()
  -- Player Inventory
  GetPlayerSlot :: Short -> Player Slot
  SetPlayerSlot :: Short -> Slot -> Player ()
  -- Breaking Blocks
  StartBreaking :: BlockCoord -> Player ()
  StopBreaking :: BlockCoord -> Player ()
  -- Sprinting and Sneaking
  SetMoveMode :: MoveMode -> Player ()
  GetMoveMode :: Player MoveMode
  -- Flush Inbox
  FlushInbox :: Player ()

getBrand :: HasPlayer r => Eff r (Maybe String)
getBrand = send PlayerBrand

setBrand :: HasPlayer r => String -> Eff r ()
setBrand = send . SetPlayerBrand

getHolding :: HasPlayer r => Eff r Short
getHolding = send PlayerHolding

setHolding :: HasPlayer r => Short -> Eff r ()
setHolding = send . SetPlayerHolding

pendTeleport :: HasPlayer r => (Double,Double,Double) -> (Float,Float) -> Word8 -> Eff r ()
pendTeleport xyz yp r = send (PlayerAddTP xyz yp r)

clearTeleport :: HasPlayer r => VarInt -> Eff r Bool
clearTeleport = send . PlayerClearTP

pendKeepAlive :: HasPlayer r => Eff r ()
pendKeepAlive = send PlayerAddKeepAlive

clearKeepAlive :: HasPlayer r => VarInt -> Eff r Bool
clearKeepAlive = send . PlayerClearKeepAlive

initPlayer :: (HasWorld r, HasNetworking r, HasLogging r) => Eff (Player ': r) a -> Eff r a
initPlayer = (newPlayer >>=) . flip runPlayer

getGamemode :: HasPlayer r => Eff r Gamemode
getGamemode = send PlayerGamemode

setGamemode :: HasPlayer r => Gamemode -> Eff r ()
setGamemode = send . SetPlayerGamemode

setPlayerPos :: HasPlayer r => (Double,Double,Double) -> Eff r ()
setPlayerPos = send . SetPlayerPosition

getPlayerPos :: HasPlayer r => Eff r (Double,Double,Double)
getPlayerPos = send GetPlayerPosition

setPlayerViewAngle :: HasPlayer r => (Float,Float) -> Eff r ()
setPlayerViewAngle = send . SetPlayerViewAngle

getPlayerViewAngle :: HasPlayer r => Eff r (Float,Float)
getPlayerViewAngle = send GetPlayerViewAngle

setInventorySlot :: HasPlayer r => Short -> Slot -> Eff r ()
setInventorySlot a b = send $ SetPlayerSlot a b

getMoveMode :: HasPlayer r => Eff r MoveMode
getMoveMode = send GetMoveMode

setMoveMode :: HasPlayer r => MoveMode -> Eff r ()
setMoveMode = send . SetMoveMode

flushInbox :: HasPlayer r => Eff r ()
flushInbox = send FlushInbox

-- Hotbar: 0-8
-- Inventory: 9-35
-- Armor: Head to Toe: 36,37,38,39
-- Crafting: output: 40, inputs: tl: 41, tr: 42, bl: 43, br: 44
-- off hand: 45
getInventorySlot :: HasPlayer r => Short -> Eff r Slot
getInventorySlot = send . GetPlayerSlot

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

-- TODO: Check if bad things can happen in between getPlayer and setPlayer here. What if the MVar changes in the meantime?
runPlayer :: (HasNetworking r, HasWorld r, HasLogging r) => PlayerId -> Eff (Player ': r) a -> Eff r a
runPlayer _ (Pure x) = return x
runPlayer i (Eff u q) = case u of
  -- Get the player's client brand
  Inject PlayerBrand -> getPlayer i >>= runPlayer i . runTCQ q . clientBrand
  -- Set the player's client brand
  Inject (SetPlayerBrand b) -> getPlayer i >>= \p -> setPlayer i p {clientBrand = Just b} >> runPlayer i (runTCQ q ())
  -- Get the player's selected slot
  Inject PlayerGamemode -> getPlayer i >>= runPlayer i . runTCQ q . gameMode
  -- Set the player's selected slot
  Inject (SetPlayerGamemode g) -> do
    p <- getPlayer i 
    setPlayer i p {gameMode = g}
    -- 0x03 means "Change gamemode"
    sendPacket (Client.ChangeGameState (ChangeGamemode g))
    runPlayer i (runTCQ q ())
  -- Get the player's selected slot
  Inject PlayerHolding -> getPlayer i >>= runPlayer i . runTCQ q . holdingSlot
  -- Set the player's selected slot
  Inject (SetPlayerHolding s) -> getPlayer i >>= \p -> setPlayer i p {holdingSlot = s} >> runPlayer i (runTCQ q ())
  -- Add a new tid to the que
  Inject (PlayerAddTP xyz yp relFlag) -> getPlayer i >>= \p -> do
    sendPacket (Client.PlayerPositionAndLook xyz yp relFlag (nextTid p))
    setPlayer i p {nextTid = 1 + nextTid p,teleportConfirmationQue = Set.insert (nextTid p) $ teleportConfirmationQue p}
    runPlayer i (runTCQ q ())
  -- Check if the tid is in the que. If it is, then clear and return true, else false
  Inject (PlayerClearTP tid) -> getPlayer i >>= \p -> do
    setPlayer i p {teleportConfirmationQue = Set.delete tid $ teleportConfirmationQue p}
    runPlayer i (runTCQ q $ Set.member tid $ teleportConfirmationQue p)
  -- Add a new keep alive id to the que
  Inject PlayerAddKeepAlive -> getPlayer i >>= \p -> do
    setPlayer i p {nextKid = 1 + nextKid p, keepAliveQue = Set.insert (nextKid p) $ keepAliveQue p}
    runPlayer i (runTCQ q ())
  -- Check if the keep alive id is in the que. If it is, then clear and return true, else false
  Inject (PlayerClearKeepAlive k) -> getPlayer i >>= \p -> do
    setPlayer i p {keepAliveQue = Set.delete k $ keepAliveQue p}
    runPlayer i (runTCQ q $ Set.member k $ keepAliveQue p)
  Inject GetPlayerPosition -> getPlayer i >>= runPlayer i . runTCQ q . playerPosition
  Inject (SetPlayerPosition xyz) -> getPlayer i >>= \p -> setPlayer i p {playerPosition = xyz} >> runPlayer i (runTCQ q ())
  Inject GetPlayerViewAngle -> getPlayer i >>= runPlayer i . runTCQ q . viewAngle
  Inject (SetPlayerViewAngle yp) -> getPlayer i >>= \p -> setPlayer i p {viewAngle = yp} >> runPlayer i (runTCQ q ())
  Inject (GetPlayerSlot slotNum) -> getPlayer i >>= runPlayer i . runTCQ q . Map.findWithDefault EmptySlot slotNum . playerInventory
  Inject (SetPlayerSlot slotNum slot) -> do
    p <- getPlayer i
    setPlayer i p {playerInventory = Map.insert slotNum slot (playerInventory p)}
    sendPacket (Client.SetSlot 0 (civskellToClientSlot slotNum) slot)
    runPlayer i (runTCQ q ())
  Inject (StartBreaking block) -> getPlayer i >>= \p -> setPlayer i p {diggingBlocks = Map.insert block (BlockBreak 0) (diggingBlocks p)} >> runPlayer i (runTCQ q ())
  Inject (StopBreaking block) -> getPlayer i >>= \p -> setPlayer i p {diggingBlocks = Map.delete block (diggingBlocks p)} >> runPlayer i (runTCQ q ())
  Inject GetMoveMode -> getPlayer i >>= runPlayer i . runTCQ q . moveMode
  Inject (SetMoveMode mode) -> getPlayer i >>= \p -> setPlayer i p {moveMode = mode} >> runPlayer i (runTCQ q ())
  Inject FlushInbox -> do
    pkts <- inboxForPlayer i
    forM_ pkts sendPacket 
    runPlayer i (runTCQ q ())
  -- Not our turn
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runPlayer i (runTCQ q x)))

