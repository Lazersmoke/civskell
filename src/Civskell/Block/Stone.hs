{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Civskell.Block.Stone where

import Civskell.Data.Types
import Civskell.Block
import Civskell.Block.Cobblestone

-- Bool isPolished
data Stone (t :: AsType) = Stone | Granite Bool | Diorite Bool | Andesite Bool
instance Block (Stone 'AsBlock) where
  blockId = 1
  blockIdentifier = "minecraft:stone"
  blockMeta = \case
    Stone -> 0
    Granite False -> 1
    Granite True -> 2
    Diorite False -> 3
    Diorite True -> 4
    Andesite False -> 5
    Andesite True -> 6
  blockName = \case
    Stone -> "Stone"
    Granite False -> "Granite"
    Granite True -> "Polished Granite"
    Diorite False -> "Diorite"
    Diorite True -> "Polished Diorite"
    Andesite False -> "Andesite"
    Andesite True -> "Polished Andesite"
  droppedItem = some . \case
    Stone -> Cobblestone :: Cobblestone 'AsItem
    Granite False -> Granite False :: Stone 'AsItem
    Granite True -> Granite True :: Stone 'AsItem
    Diorite False -> Diorite False :: Stone 'AsItem
    Diorite True -> Diorite True :: Stone 'AsItem
    Andesite False -> Andesite False :: Stone 'AsItem
    Andesite True -> Andesite True :: Stone 'AsItem

instance Item (Stone 'AsItem) where
  itemId = 0x1
  itemIdentifier = "minecraft:stone"
  onItemUse = Just placeBlock
