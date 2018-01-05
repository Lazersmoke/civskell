{-# LANGUAGE OverloadedStrings #-}
-- | The item entity
module Civskell.Entity.Item where

import Civskell.Entity.Base
import Civskell.Data.Types

-- | Item entity. Can't be empty
data ItemEntity i = ItemEntity BaseEntity (SlotData i)

-- | Trivial instance
instance Object (ItemEntity i) where
  objectName = "Item Stack (Slot)"
  objectId = 2
  objectData (ItemEntity e _) = (1,Just (baseEntityVelocity e))

-- | Trivial instance
instance Entity (ItemEntity i) where
  entityName = "ItemEntity"
  entityType = 1
  entitySize _ = (0.25,0.25,0.25)
  entityLocation (ItemEntity e _) = baseEntityLocation e
  entityVelocity (ItemEntity e _) = baseEntityVelocity e
  entityMeta (ItemEntity e slotData) = baseEntityMeta e ++ [mm . toWireSlot $ Slot (Just slotData)]


