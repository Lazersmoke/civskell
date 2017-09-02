{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Civskell.Block.Stone where

import Unsafe.Coerce (unsafeCoerce)

import Civskell.Data.Types
import Civskell.Data.Block

-- Bool isPolished
data Stone (t :: AsType) = Stone | Granite Bool | Diorite Bool | Andesite Bool

convStone :: forall b a. Stone a -> Stone b
convStone = unsafeCoerce

stoneToBlock :: Item (Stone 'AsItem) -> Block (Stone 'AsBlock)
stoneToBlock (Item _itemStone s) = Block stone (convStone s)

stone :: BlockDescriptor (Stone 'AsBlock)
stone = BlockDescriptor
  {blockId = 1
  ,blockIdentifier = "minecraft:stone"
  ,blockMeta = \case
    Stone -> 0
    Granite False -> 1
    Granite True -> 2
    Diorite False -> 3
    Diorite True -> 4
    Andesite False -> 5
    Andesite True -> 6
  ,blockName = \case
    Stone -> "Stone"
    Granite False -> "Granite"
    Granite True -> "Polished Granite"
    Diorite False -> "Diorite"
    Diorite True -> "Polished Diorite"
    Andesite False -> "Andesite"
    Andesite True -> "Polished Andesite"
  ,onClick = Nothing
  --,droppedItem = Just $ some . convStone @'AsItem
  }

itemStone :: ItemDescriptor (Stone 'AsItem)
itemStone = ItemDescriptor
  {itemId = 0x1
  ,itemIdentifier = "minecraft:stone"
  ,itemMeta = const 0
  ,itemNBT = const Nothing
  ,onItemUse = placeBlock stoneToBlock
  --,parseItem = standardParser (Stone @'AsItem)
  }

