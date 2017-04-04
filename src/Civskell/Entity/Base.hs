module Civskell.Entity.Base where

import Civskell.Data.Types
import Data.SuchThat
import Data.Word (Word8)

-- Stored in class Mob:
--   Entity Type data: Name, Mob Type
--
-- Stored in data Creeper:
--   Instance data: (x,y,z) (yaw,pitch) charged or not charged
--
-- setProp :: forall p m t. HasProperty m t p => t -> m -> m

mm :: EntityMetaType p => p -> EntityMetadata
mm = SuchThat . EntityMeta

-- Bitmask
-- 0x01 NoAI
-- 0x02 Left handed
data Insentient = Insentient Living Word8

defaultInsentient :: Insentient
defaultInsentient = Insentient defaultLiving 0x00

insentientLocation :: Insentient -> EntityLocation
insentientLocation (Insentient l _) = livingLocation l

insentientVelocity :: Insentient -> EntityVelocity
insentientVelocity (Insentient l _) = livingVelocity l

insentientMeta :: Insentient -> [EntityMetadata]
insentientMeta (Insentient l bm) = livingMeta l ++ [mm bm]

-- 0x01 Is hand active?
-- 0x02 Active hand -- WTF is this even saying?
-- TODO: Enums
data Living = Living BaseEntity Word8 Float VarInt Bool VarInt

defaultLiving :: Living
defaultLiving = Living defaultEntity 0x00 1.0 0 False 0

livingLocation :: Living -> EntityLocation
livingLocation (Living b _ _ _ _ _) = baseEntityLocation b

livingVelocity :: Living -> EntityVelocity
livingVelocity (Living b _ _ _ _ _) = baseEntityVelocity b

livingMeta :: Living -> [EntityMetadata]
livingMeta (Living e handBM hp pots ambient arrowsIn) = baseEntityMeta e ++ [mm handBM,mm hp,mm pots,mm ambient,mm arrowsIn]

data BaseEntity = BaseEntity EntityLocation EntityVelocity Word8 VarInt String Bool Bool Bool

defaultEntity :: BaseEntity
defaultEntity = BaseEntity (EntityLocation (0,130,0) (0,0)) (EntityVelocity (0,0,0)) 0x00 300 "" False False False

baseEntityLocation :: BaseEntity -> EntityLocation
baseEntityLocation (BaseEntity l _ _ _ _ _ _ _) = l

baseEntityVelocity :: BaseEntity -> EntityVelocity
baseEntityVelocity (BaseEntity _ v _ _ _ _ _ _) = v

baseEntityMeta :: BaseEntity -> [EntityMetadata]
baseEntityMeta (BaseEntity _ _ statusBM air name nameVisible silent noGrav) = [mm statusBM,mm air,mm name,mm nameVisible,mm silent,mm noGrav]


