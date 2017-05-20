{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Civskell.Block.Cobblestone where

import Civskell.Data.Types
import Civskell.Block

data Cobblestone (t :: AsType) = Cobblestone
instance Block (Cobblestone 'AsBlock) where
  blockId = 4
  blockIdentifier = "minecraft:cobblestone"
  blockMeta Cobblestone = 0
  blockName Cobblestone = "Cobblestone"
  droppedItem = Just $ \Cobblestone -> some (Cobblestone :: Cobblestone 'AsItem)

instance Item (Cobblestone 'AsItem) where
  itemId = 4
  itemIdentifier = "minecraft:cobblestone"
  onItemUse = Just $ \Cobblestone -> placeBlock (Cobblestone :: Cobblestone 'AsBlock)
