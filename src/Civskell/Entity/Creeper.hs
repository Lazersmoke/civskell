{-# LANGUAGE OverloadedStrings #-}
module Civskell.Entity.Creeper where

import Civskell.Entity.Base
import Civskell.Data.Types

data Creeper = Creeper Insentient VarInt Bool Bool
instance Mob Creeper where {}
instance Entity Creeper where
  entityName = "Creeper"
  entityType = 50
  entitySize _ = (0.6,1.7,0.6)
  entityLocation (Creeper i _ _ _) = insentientLocation i
  entityVelocity (Creeper i _ _ _) = insentientVelocity i
  entityMeta (Creeper i fuse charge ignite) = insentientMeta i ++ [mm fuse,mm charge,mm ignite]


