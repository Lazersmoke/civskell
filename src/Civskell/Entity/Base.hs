-- | Base entities
module Civskell.Entity.Base where

import Civskell.Data.Types
import Data.SuchThat
import Data.Word (Word8)

-- | Makes an @'EntityMetadata'@ from an appropriate @p@ with a @'EntityMetaType'@ instance.
mm :: EntityMetaType p => p -> EntityMetadata
mm = SuchThat . EntityMeta

-- Bitmask
-- 0x01 NoAI
-- 0x02 Left handed
-- | An insentient entity
data Insentient = Insentient Living Word8

-- | The default insentient entity
defaultInsentient :: Insentient
defaultInsentient = Insentient defaultLiving 0x00

-- | A @'Insentient'@'s location
insentientLocation :: Insentient -> EntityLocation
insentientLocation (Insentient l _) = livingLocation l

-- | A @'Insentient'@'s velocity
insentientVelocity :: Insentient -> EntityVelocity
insentientVelocity (Insentient l _) = livingVelocity l

-- | A @'Insentient'@'s metadata
insentientMeta :: Insentient -> [EntityMetadata]
insentientMeta (Insentient l bm) = livingMeta l ++ [mm bm]

-- 0x01 Is hand active?
-- 0x02 Active hand -- WTF is this even saying?
-- TODO: Enums
-- | A living entity
data Living = Living BaseEntity Word8 Float VarInt Bool VarInt

-- | The default living entity
defaultLiving :: Living
defaultLiving = Living defaultEntity 0x00 1.0 0 False 0

-- | A @'Living'@'s location
livingLocation :: Living -> EntityLocation
livingLocation (Living b _ _ _ _ _) = baseEntityLocation b

-- | A @'Living'@'s velocity
livingVelocity :: Living -> EntityVelocity
livingVelocity (Living b _ _ _ _ _) = baseEntityVelocity b

-- | A @'Living'@'s metadata
livingMeta :: Living -> [EntityMetadata]
livingMeta (Living e handBM hp pots ambient arrowsIn) = baseEntityMeta e ++ [mm handBM,mm hp,mm pots,mm ambient,mm arrowsIn]

-- | The minimal spec for an entity
data BaseEntity = BaseEntity EntityLocation EntityVelocity Word8 VarInt String Bool Bool Bool

-- | The default entity
defaultEntity :: BaseEntity
defaultEntity = BaseEntity (EntityLocation (0,130,0) (0,0)) (EntityVelocity (0,0,0)) 0x00 300 "" False False False

-- | A @'BaseEntity'@'s location
baseEntityLocation :: BaseEntity -> EntityLocation
baseEntityLocation (BaseEntity l _ _ _ _ _ _ _) = l

-- | A @'BaseEntity'@'s velocity
baseEntityVelocity :: BaseEntity -> EntityVelocity
baseEntityVelocity (BaseEntity _ v _ _ _ _ _ _) = v

-- | A @'BaseEntity'@'s metadata
baseEntityMeta :: BaseEntity -> [EntityMetadata]
baseEntityMeta (BaseEntity _ _ statusBM airRemaining name nameVisible silent noGrav) = [mm statusBM,mm airRemaining,mm name,mm nameVisible,mm silent,mm noGrav]
