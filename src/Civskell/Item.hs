{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Items
module Civskell.Item where

--import Data.Word (Word8)
--import qualified Data.Map.Lazy as Map
--import Data.Bytes.Serial
--import Data.Bytes.Put
--import Data.Functor.Identity
--import Control.Monad.Freer
--import Data.SuchThat
--import Data.NBT
--import qualified Data.Serialize as Cereal

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
{-
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
-}
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
