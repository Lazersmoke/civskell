{-# LANGUAGE OverloadedStrings #-}
module Civskell.Entity.Item where

import Civskell.Entity.Base
import Civskell.Data.Types

data ItemEntity i = ItemEntity BaseEntity (SlotData i)

instance Object (ItemEntity i) where
  objectName = "Item Stack (Slot)"
  objectId = 2
  objectData (ItemEntity e _) = (1,Just (baseEntityVelocity e))

instance Entity (ItemEntity i) where
  entityName = "ItemEntity"
  entityType = 1
  entitySize _ = (0.25,0.25,0.25)
  entityLocation (ItemEntity e _) = baseEntityLocation e
  entityVelocity (ItemEntity e _) = baseEntityVelocity e
  entityMeta (ItemEntity e slotData) = baseEntityMeta e ++ [mm . toWireSlot $ Slot (Just slotData)]


