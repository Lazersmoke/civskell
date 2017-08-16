{-# LANGUAGE OverloadedStrings #-}
module Civskell.Entity.Item where

import Civskell.Entity.Base
import Civskell.Data.Types
import Civskell.Item (SlotData)

data Item = Item BaseEntity SlotData

instance Object Item where
  objectName = "Item Stack (Slot)"
  objectId = 2
  objectData (Item e _) = (1,Just (baseEntityVelocity e))

instance Entity Item where
  entityName = "Item"
  entityType = 1
  entitySize _ = (0.25,0.25,0.25)
  entityLocation (Item e _) = baseEntityLocation e
  entityVelocity (Item e _) = baseEntityVelocity e
  entityMeta (Item e slotData) = baseEntityMeta e ++ [mm slotData]


