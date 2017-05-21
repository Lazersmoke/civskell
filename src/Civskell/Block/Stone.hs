{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Civskell.Block.Stone where

import Civskell.Data.Types
import Civskell.Block
--import Civskell.Block.Cobblestone

-- Bool isPolished
data Stone (t :: AsType) = Stone | Granite Bool | Diorite Bool | Andesite Bool

convStone :: forall b a. Stone a -> Stone b
convStone = \case
    Stone -> Stone :: Stone b
    Granite False -> Granite False :: Stone b
    Granite True -> Granite True :: Stone b
    Diorite False -> Diorite False :: Stone b
    Diorite True -> Diorite True :: Stone b
    Andesite False -> Andesite False :: Stone b
    Andesite True -> Andesite True :: Stone b

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
  droppedItem = Just $ some . convStone @'AsItem

instance Item (Stone 'AsItem) where
  itemId = 0x1
  itemIdentifier = "minecraft:stone"
  onItemUse = Just (placeBlock . convStone @'AsBlock)
  parseItem = standardParser (Stone @'AsItem)
