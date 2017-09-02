{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Window where

import qualified Data.Set as Set
import Control.Concurrent.STM
import Control.Lens
import Data.SuchThat
import Control.Monad.Freer
import Control.Monad.Freer.State
import qualified Data.Text as T
import Data.Semigroup ((<>))

import Civskell.Data.Types

-- Given a way to get a slot, a way to set a slot, a window to use, a slot number, and a transaction id, click the slot
defaultInventoryClick :: Members '[Packeting,Logging,WorldManipulation,PlayerManipulation] r => (Short -> Eff r (ForAny Slot)) -> (forall i. Short -> Slot i -> Eff r ()) -> WindowId -> Short -> TransactionId -> InventoryClickMode -> Eff r Bool
defaultInventoryClick gs ss wid slotNum transId = \case
  -- Get the current state of affairs, and purely decide what to do with them
  NormalClick rClick -> do
   slotItem <- gs slotNum
   handItem <- gs (-1)
   case ambiguously (\si -> ambiguously (\hi -> doInventoryClick si hi rClick) handItem) slotItem of
    -- If everything is in order
    Just (newSlot,newHand) -> do
      -- Set the slots to their new values
      logp $ "Setting slot -1 to " <> T.pack (ambiguously show newHand)
      ambiguously (ss (-1)) newHand
      logp $ "Setting slot " <> T.pack (show slotNum) <> " to " <> T.pack (ambiguously show newSlot)
      ambiguously (ss slotNum) newSlot
      -- Confirm the transaction was successful
      return True --sendPacket (Client.ConfirmTransaction wid transId True)
    -- If something went wrong
    Nothing -> do
      -- Log to console
      loge "Failed to confirm client transaction"
      modify $ playerFailedTransactions %~ Set.insert (wid,transId)
      -- And tell the client it should say sorry
      return False -- sendPacket (Client.ConfirmTransaction wid transId False)
  -- Right click is exactly the same as left when shiftclicking
  -- TODO: implement the rest of the clicking bs that minecraft does
  ShiftClick _rClick -> pure False
  NumberKey _num -> pure False
  MiddleClick -> pure False
  ItemDropOut _isStack -> pure False
  -- "Painting" mode
  PaintingMode _mode -> loge "Painting mode not supported" >> return False
  -- Double click
  DoubleClick -> loge "Double click not supported" >> return False

-- Given what the slot is right now, and what the item held in the hand is, and whether they are left or right clicking
-- give back a (new slot,new held item) if it worked, or Nothing if it didn't
doInventoryClick :: Slot i -> Slot j -> Bool -> Maybe (ForAny Slot,ForAny Slot)
doInventoryClick actualSlot currHeld rClick = case actualSlot of
  Slot Nothing -> case currHeld of
    -- Both empty -> No-op
    Slot Nothing -> Just (ambiguate actualSlot,ambiguate currHeld)
    -- Putting something in an empty slot
    Slot (Just (SlotData curri currcount)) -> Just (ambiguate $ Slot placed,ambiguate $ Slot newHeld)
      where
        -- If we right click, only place one item, left click places all of them
        delta = if rClick then 1 else 64
        -- If we don't have enough items to fill the delta, place them all and end up with an Nothing, otherwise remove the delta
        newHeld = if currcount <= delta then Nothing else Just $ SlotData curri (currcount - delta)
        -- The slot now has either the full delta, or our best attempt at filling the delta
        placed = Just $ SlotData curri (min delta currcount)
  Slot (Just (SlotData (Item actdesc acti) actcount)) -> case currHeld of
    -- Picking something up into an empty hand
    Slot Nothing -> Just (ambiguate $ Slot left,ambiguate $ Slot $ Just picked)
      where
        -- If we right click, take half, otherwise take as much as we can
        delta = if rClick then actcount `div` 2 else min actcount 64
        -- If we took it all, leave nothing, otherwise take what we took
        left = if actcount == delta then Nothing else Just $ SlotData (Item actdesc acti) (actcount - delta)
        -- We are now holding everything we picked up
        picked = SlotData (Item actdesc acti) delta
    -- Two item stacks interacting
    Slot (Just (SlotData (Item currdesc curri) currcount)) -> case itemId currdesc == itemId actdesc && itemMeta currdesc curri == itemMeta actdesc acti && itemNBT currdesc curri == itemNBT actdesc acti of
      -- Like stacks; combine
      True -> Just (ambiguate $ Slot inSlot,ambiguate $ Slot stillHeld)
        where
          -- How many items we can possibly place in the slot
          spaceRemaining = 64 - actcount
          -- If we right click, try to put one in, but put zero in if its already full, otherwise put as many as we can in
          -- NOTE: delta <= currcount and spaceRemaining
          delta = if rClick then min 1 spaceRemaining else max currcount spaceRemaining
          -- If we put down everything, empty our hand, otherwise remove what we put down
          stillHeld = if currcount == delta then Nothing else Just $ SlotData (Item currdesc curri) (currcount - delta)
          -- Put the stuff we put into the slot, into the slot
          inSlot = Just $ SlotData (Item actdesc acti) (actcount + delta)
      -- Unlike stacks; swap
      False -> Just (ambiguate currHeld,ambiguate actualSlot)


-- Hotbar: 0-8
-- Inventory: 9-35
-- Armor: Head to Toe: 36,37,38,39
-- Crafting: output: 40, inputs: tl: 41, tr: 42, bl: 43, br: 44
-- off hand: 45

data Player = Player
player :: WindowDescriptor Player
player = WindowDescriptor
  {windowName = "Player Inventory"
  ,windowIdentifier = error "No identitifer for Player Inventory (Civskell.Window)"
  ,slotCount {-(Player _)-} = 45
  -- TODO: Also send the packet on set
  ,onWindowClick = \Player -> defaultInventoryClick getInventorySlot setInventorySlot
  }
  {-clientToCivskellSlot s
    | s == (-1) = s
    | s <= 4 = s + 40
    | s <= 8 = s + 31
    | s <= 35 = s
    | s <= 44 = s - 36
    | s == 45 = s
    | otherwise = error "Bad Slot Number"
  civskellToClientSlot s
    | s == (-1) = s
    | s <= 8 = s + 36
    | s <= 35 = s
    | s <= 39 = s - 31
    | s <= 44 = s - 40
    | s == 45 = s
    | otherwise = error "Bad Slot Number"
-}
-- Chests are numbered *after* the off hand slot (45)
-- Num rows (9 per row)
data Chest = Chest (TVar Inventory)
chest :: WindowDescriptor Chest
chest = WindowDescriptor
  {windowName = "Chest"
  ,windowIdentifier = "minecraft:chest"
  ,slotCount {-(Chest rows _)-} = 27 -- rows * 9
  ,onWindowClick = \(Chest i) -> let
    gsChest slotNum = if slotNum > 26 || slotNum == (-1)
      then getInventorySlot (slotNum - 18)
      else getSlot slotNum <$> send (atomically $ readTVar i)
    ssChest slotNum s' = if slotNum > 26 || slotNum == (-1)
      then setInventorySlot (slotNum - 18) s'
      else send . atomically $ modifyTVar i (setSlot slotNum s')
    in defaultInventoryClick gsChest ssChest
  }
 {- clientToCivskellSlot s
    | s < slotCount @Chest = s + 45
    | otherwise = clientToCivskellSlot @Player s
  civskellToClientSlot s
    | s > 45 = s - 45
    | otherwise = civskellToClientSlot @Player s
-}
{-
data CraftingTable = CraftingTable deriving Show
instance Window CraftingTable where
  windowName = "Crafting Table"
  windowIdentifier = "minecraft:crafting_table"
  slotCount _ = 0

data Furnace = Furnace deriving Show
instance Window Furnace where
  windowName = "Furnace"
  windowIdentifier = "minecraft:furnace"
  slotCount _ = 0

data Dispenser = Dispenser deriving Show
instance Window Dispenser where
  windowName = "Dispenser"
  windowIdentifier = "minecraft:dispenser"
  slotCount _ = 9

data EnchTable = EnchTable deriving Show
instance Window EnchTable where
  windowName = "Enchanting Table"
  windowIdentifier = "minecraft:enchanting_table"
  slotCount _ = 0

data BrewingStand = BrewingStand deriving Show
instance Window BrewingStand where
  windowName = "Brewing Stand"
  windowIdentifier = "minecraft:brewing_stand"
  slotCount _ = 5

data Villager = Villager deriving Show
instance Window Villager where
  windowName = "Villager"
  windowIdentifier = "minecraft:villager"
  slotCount _ = 0

data Beacon = Beacon deriving Show
instance Window Beacon where
  windowName = "Beacon"
  windowIdentifier = "minecraft:beacon"
  slotCount _ = 1

data Anvil = Anvil deriving Show
instance Window Anvil where
  windowName = "Anvil"
  windowIdentifier = "minecraft:anvil"
  slotCount _ = 0

data Hopper = Hopper deriving Show
instance Window Hopper where
  windowName = "Hopper"
  windowIdentifier = "minecraft:hopper"
  slotCount _ = 5

data Dropper = Dropper deriving Show
instance Window Dropper where
  windowName = "Dropper"
  windowIdentifier = "minecraft:dropper"
  slotCount _ = 9

data ShulkerBox = ShulkerBox deriving Show
instance Window ShulkerBox where
  windowName = "Shulker Box"
  windowIdentifier = "minecraft:shulker_box"
  slotCount _ = 27

-- NYI
--data HorseInv = HorseInv deriving Showderiving Show
--instance Window HorseInv where
--  windowName = "Horse Inventory"
--  windowIdentifier = "EntityHorse"
--  slotCount _ = 
-}
