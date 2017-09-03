{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
-- | Provides the @'Stone'@ block type.
module Civskell.Block.Stone where

import Unsafe.Coerce (unsafeCoerce)

import Civskell.Data.Types

-- | Type of Minecraft stone, both for items and for blocks, depending on the type parameter.
--
-- The @'Bool'@s in the constructors are @'True'@ if polished or @'False'@ if not.
data Stone (t :: AsType) = Stone | Granite Bool | Diorite Bool | Andesite Bool

-- TODO: Don't use unsafeCoerce here
-- | Convert one kind of stone to another.
-- 
-- > convStone :: Stone 'AsItem -> Stone 'AsBlock
-- > convStone :: Stone 'AsBlock -> Stone 'AsItem
convStone :: forall b a. Stone a -> Stone b
convStone = unsafeCoerce

-- | Convert a @'Item' ('Stone' ''AsItem')@ to a @'Block' ('Stone' ''AsBlock')@ using the standard @'stone'@ descriptor.
stoneToBlock :: Item (Stone 'AsItem) -> Block (Stone 'AsBlock)
stoneToBlock (Item _itemStone s) = Block stone (convStone s)

-- | Standard descriptor for Minecraft stone block.
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
  }

-- | Standard descriptor for Minecraft stone item.
itemStone :: ItemDescriptor (Stone 'AsItem)
itemStone = ItemDescriptor
  {itemId = 0x1
  ,itemIdentifier = "minecraft:stone"
  ,itemMeta = const 0
  ,itemNBT = const Nothing
  ,onItemUse = placeBlock stoneToBlock
  }

