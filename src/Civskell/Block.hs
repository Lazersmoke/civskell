{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Civskell.Block where

import Control.Eff
import Data.Attoparsec.ByteString
import Civskell.Data.Player
import Civskell.Data.World
import Civskell.Data.Logging
import Control.Monad

import Civskell.Tech.Parse
import Civskell.Data.Types

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
  return . Slot . Just $ SlotData (some r) cnt


placeBlock :: (HasWorld r,SendsPackets r,HasPlayer r,Logs r,Block b) => b -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Eff r ()
placeBlock b bc bf hand _ = do
  -- Find out what item they are trying to place
  heldSlot <- if hand == MainHand then (+36) . holdingSlot <$> getPlayer else pure 45
  getInventorySlot heldSlot >>= \case
    Slot Nothing -> loge "Fight me; this is not possible! (literally Nothing has a `placeBlock` callback)"
    Slot (Just (SlotData i icount)) -> do
      -- Remove item from inventory
      let newSlot = if icount == 1 then Slot Nothing else Slot $ Just (SlotData i (icount - 1))
      setInventorySlot heldSlot newSlot
      setBlock b (blockOnSide bc bf)


