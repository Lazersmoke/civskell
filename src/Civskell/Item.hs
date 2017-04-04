{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Item where

import Data.Attoparsec.ByteString
import Control.Eff
--import Data.NBT
import Control.Monad
import qualified Data.Map as Map

import Civskell.Tech.Parse
import Civskell.Data.Types
import Civskell.Data.Player
import Civskell.Data.World
import Civskell.Data.Logging
import qualified Civskell.Tile as Tile

parseSlot :: Parser Slot
parseSlot = choice
  [emptySlot
  ,parseItem @Stick
  ,parseItem @Stone
  ,parseItem @Chest
  ]

emptySlot :: Parser Slot
emptySlot = do
  bid <- parseShort
  guard $ bid == -1
  return EmptySlot

-- Parse an item based on its blockId, assuming no metadata and no nbt
standardParser :: forall i. Item i => i -> Parser Slot
standardParser r = do
  bid <- parseShort
  guard $ bid == itemId @i
  cnt <- anyWord8
  dmg <- parseShort
  guard $ dmg == 0
  nbtFlair <- anyWord8
  guard $ nbtFlair == 0
  return $ Slot (some r) cnt

placeBlock :: (HasWorld r,SendsPackets r,HasPlayer r,Logs r,Block b) => b -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Eff r ()
placeBlock b bc bf hand _ = do
  -- Find out what item they are trying to place
  heldSlot <- if hand == MainHand then (+36) . holdingSlot <$> getPlayer else pure 45
  getInventorySlot heldSlot >>= \case
    EmptySlot -> loge "Impossible: Player is placing an empty slot as a block!!!"
    Slot i icount -> do
      -- Remove item from inventory
      let newSlot = if icount == 1 then EmptySlot else (Slot i (icount - 1))
      setInventorySlot heldSlot newSlot
      setBlock b (blockOnSide bc bf)

data Stick = Stick
instance Item Stick where
  itemId = 0x118
  itemIdentifier = "minecraft:stick"
  parseItem = standardParser Stick

data Stone = Stone
instance Item Stone where
  itemId = 0x1
  itemIdentifier = "minecraft:stone"
  parseItem = standardParser Stone
  onItemUse = Just $ \Stone -> placeBlock Tile.Stone

data Chest = Chest
instance Item Chest where
  itemId = 0x36
  itemIdentifier = "minecraft:chest"
  parseItem = standardParser Chest
  -- TODO: Directionalize
  onItemUse = Just $ \Chest bc bf hand fff -> send (WorldNewTVar Map.empty) >>= \items -> placeBlock (Tile.Chest North items) bc bf hand fff