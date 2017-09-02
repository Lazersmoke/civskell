{-# LANGUAGE RankNTypes #-}
module Civskell.Data.Block where

import Control.Lens
import Data.SuchThat
import Control.Monad.Freer.State

import Civskell.Data.Types

placeBlock :: (Item i -> Block b) -> ItemUseCallback i
placeBlock f = Just (\(SlotData i ic) -> Slot . Just $ SlotData i (ic - 1),\i bc bf _hand _blockPart -> modify $ blockInWorld (blockOnSide bc bf) .~ ambiguate (f i))
