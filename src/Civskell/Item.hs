{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Item where

import Data.Attoparsec.ByteString (Parser,choice)
import Data.Word (Word8)
import qualified Data.Map.Lazy as Map
import Control.Monad
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Functor.Identity
import Control.Monad.Freer
import Data.SuchThat
import Data.NBT
import qualified Data.Serialize as Cereal

import Civskell.Data.Types
import Civskell.Tech.Parse
{-
import Control.Concurrent.STM
import Data.NBT
import Control.Eff
import qualified Data.Map as Map
import Civskell.Data.Player
import Civskell.Data.World
import Civskell.Data.Logging
import qualified Civskell.Tile as Tile
-}
--import Civskell.Block.Stone

class Item i where
  -- TODO: Type level this when we have dependent types
  itemId :: Short
  itemIdentifier :: String
  itemMeta :: i -> Short
  itemMeta _ = 0
  --itemPlaced :: i -> Maybe (Some Block)
  --itemPlaced _ = Nothing
  itemNBT :: i -> Maybe NbtContents
  itemNBT _ = Nothing
  -- TODO: param Slot i
  parseItem :: Parser Slot
  -- Some items do something when right clicked
  onItemUse :: forall r. ({-HasWorld r,HasPlayer r,-}SendsPackets r,Logs r) => Maybe (i -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Eff r ())
  onItemUse = Nothing

newtype Slot = Slot (Maybe SlotData)

instance Show Slot where
  show (Slot (Just dat)) = "{" ++ show dat ++ "}"
  show (Slot Nothing) = "{}"

data SlotData = SlotData (Some Item) Word8

instance EntityMetaType SlotData where entityMetaFlag = 0x05
instance Serial (EntityMeta SlotData) where {}

-- An inventory is a mapping from Slot numbers to Slot data (or lack thereof).
-- TODO: Reconsider sparsity obligations of Map here, and decide on a better
-- internal representation. Potentially make this abstract after all. There
-- might be a really fitting purely functional data structure to show off here.
type Inventory = Map.Map Short Slot

-- Abstraction methods for Inventory, not really needed tbh.
getSlot :: Short -> Inventory -> Slot
getSlot = Map.findWithDefault (Slot Nothing)

setSlot :: Short -> Slot -> Inventory -> Inventory
setSlot i (Slot Nothing) = Map.delete i
setSlot i s = Map.insert i s

-- Sidestep existential nonsense when creating items
slot :: Item i => i -> Word8 -> Slot
slot i c = Slot . Just $ SlotData (ambiguate . Identity $ i) c

-- This is a rather sketchy instance because itemId uniqueness is not enforced
-- at type level, so we could actually get two different Item instances claiming
-- the same id, meta, and NBT, which would make this `Eq` instance unreliable.
-- In fact, it might be *impossible* to write an `Eq` instance for Slots
-- involving general, open typeclass `Item`s because that would involve deciding
-- equality for a `forall a. c a => p a` sort of thing, which would only be
-- based on the `c` instance.
instance Eq SlotData where
  (SlotData (SuchThat (Identity (a :: at))) ac) == (SlotData (SuchThat (Identity (b :: bt))) bc) = itemId @at == itemId @bt && itemMeta a == itemMeta b && itemNBT a == itemNBT b && ac == bc

slotAmount :: Slot -> Word8
slotAmount (Slot (Just (SlotData _ c))) = c
slotAmount (Slot Nothing) = 0

-- Remove as many items as possible, up to count, return amount removed
removeCount :: Word8 -> Slot -> (Word8,Slot)
removeCount _ (Slot Nothing) = (0,Slot Nothing)
removeCount c (Slot (Just (SlotData i count))) = if c < count then (c,Slot . Just $ SlotData i (count - c)) else (count,Slot Nothing)

-- Count is how many to put into first stack from second
splitStack :: Word8 -> Slot -> (Slot,Slot)
splitStack _ (Slot Nothing) = (Slot Nothing,Slot Nothing)
splitStack c s@(Slot (Just (SlotData i _))) = (firstStack,secondStack)
  where
    firstStack = if amtFirstStack == 0 then Slot Nothing else Slot . Just $ SlotData i amtFirstStack
    (amtFirstStack,secondStack) = removeCount c s

-- Pretty print a slot's data. Perhaps we should let `Item`s provide a fancy name for this instance to use.
instance Show SlotData where
  show (SlotData (SuchThat (Identity (i :: it))) count) = show count ++ " of [" ++ show (itemId @it) ++ ":" ++ show (itemMeta i) ++ "]" ++ n (itemNBT i)
    where
      -- Only display the NBT if it is actually there
      n Nothing = ""
      n (Just nbt) = " with tags: " ++ show nbt

instance Serial Slot where
  serialize (Slot Nothing) = serialize (-1 :: Short)
  serialize (Slot (Just sd)) = serialize @SlotData sd
  deserialize = deserialize @Short >>= \case
    (-1) -> pure $ Slot Nothing
    _itemId -> error "Unimplemented: Deserialization of general Slots. May be unimplementable in general :/"

instance Serial SlotData where
  serialize (SlotData (SuchThat (Identity (i :: it))) count) = serialize (itemId @it) >> serialize count >> serialize (itemMeta i) >> (case itemNBT i of {Just nbt -> putByteString $ Cereal.encode (NBT "" nbt);Nothing -> putWord8 0x00})
  deserialize = error "Unimplemented: deserialize @SlotData"


parseSlot :: Parser Slot
parseSlot = choice
  [emptySlot
  --,parseItem @Stick
  --,parseItem @(Stone 'AsItem)
  --,parseItem @Chest
  ]

emptySlot :: Parser Slot
emptySlot = do
  bid <- parseShort
  guard $ bid == -1
  return $ Slot Nothing

{-
data Stick = Stick
instance Item Stick where
  itemId = 0x118
  itemIdentifier = "minecraft:stick"
  parseItem = standardParser Stick

data Chest = Chest
instance Item Chest where
  itemId = 0x36
  itemIdentifier = "minecraft:chest"
  parseItem = standardParser Chest
  -- TODO: Directionalize
  onItemUse = Just $ \Chest bc bf hand fff -> send (WorldSTM $ newTVar Map.empty) >>= \items -> placeBlock (Tile.Chest North items) bc bf hand fff
-}
