{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Civskell.Block.Cobblestone where

import Civskell.Data.Types
import Civskell.Data.Block

data Air = Air

instance Block Air where
  blockIdentifier = "minecraft:air"
  blockName _ = "Air"
  blockId = 0
  droppedItem = Nothing


