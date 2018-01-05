{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Various mobs and other entities
module Civskell.Entity.Object where

import Civskell.Entity.Base
import Civskell.Data.Types
import Data.Word (Word8)

-- | A Skeleton entity
data Skeleton = Skeleton Insentient Bool
-- | Trivial instance
instance Mob Skeleton where {}
-- | Trivial instance
instance Entity Skeleton where
  entityName = "Skeleton"
  entityType = 51
  entitySize _ = (0.6,1.99,0.6)
  entityLocation (Skeleton i _) = insentientLocation i
  entityVelocity (Skeleton i _) = insentientVelocity i
  entityMeta (Skeleton i swingArms) = insentientMeta i ++ [mm swingArms]

-- | A Spider entity
data Spider = Spider Insentient Word8
-- | Trivial instance
instance Mob Spider where {}
-- | Trivial instance
instance Entity Spider where
  entityName = "Spider"
  entityType = 52
  entitySize _ = (1.4,0.9,1.4) -- minecraft:spider
  entityLocation (Spider i _) = insentientLocation i
  entityVelocity (Spider i _) = insentientVelocity i
  entityMeta (Spider i isClimbing) = insentientMeta i ++ [mm isClimbing]

-- | A Giant entity
newtype Giant = Giant Insentient
-- | Trivial instance
instance Mob Giant where {}
-- | Trivial instance
instance Entity Giant where
  entityName = "Giant"
  entityType = 53
  entitySize _ = (0.6 * 6,1.8 * 6,0.6 * 6) -- minecraft:giant
  entityLocation (Giant i) = insentientLocation i
  entityVelocity (Giant i) = insentientVelocity i
  entityMeta (Giant i) = insentientMeta i

-- | A Zombie entity
data Zombie = Zombie Insentient Bool Bool
-- | Trivial instance
instance Mob Zombie where {}
-- | Trivial instance
instance Entity Zombie where
  entityName = "Zombie"
  entityType = 54
  entitySize _ = (0.6,1.95,0.6) --  minecraft:zombie
  entityLocation (Zombie i _ _) = insentientLocation i
  entityVelocity (Zombie i _ _) = insentientVelocity i
  entityMeta (Zombie i baby handsUp) = insentientMeta i ++ [mm baby, mm (0 :: VarInt) {- Deprecated -}, mm handsUp]

-- | A Slime entity
data Slime = Slime Insentient VarInt
-- | Trivial instance
instance Mob Slime where {}
-- | Trivial instance
instance Entity Slime where
  entityName = "Slime"
  entityType = 55
  entitySize (Slime _ size) = (0.51000005 * fromIntegral size,0.51000005 * fromIntegral size,0.51000005 * fromIntegral size)
  entityLocation (Slime i _) = insentientLocation i
  entityVelocity (Slime i _) = insentientVelocity i
  entityMeta (Slime i size) = insentientMeta i ++ [mm size]

-- | A Ghast entity
data Ghast = Ghast Insentient Bool
-- | Trivial instance
instance Mob Ghast where {}
-- | Trivial instance
instance Entity Ghast where
  entityName = "Ghast"
  entityType = 56
  entitySize _ = (4,4,4) --  minecraft:ghast
  entityLocation (Ghast i _) = insentientLocation i
  entityVelocity (Ghast i _) = insentientVelocity i
  entityMeta (Ghast i attacking) = insentientMeta i ++ [mm attacking]

-- | A PigZombie entity
newtype PigZombie = PigZombie Zombie
-- | Trivial instance
instance Mob PigZombie where {}
-- | Trivial instance
instance Entity PigZombie where
  entityName = "PigZombie"
  entityType = 57
  entitySize _ = (0.6,1.95,0.6) --  minecraft:zombie_pigman
  entityLocation (PigZombie z) = entityLocation z
  entityVelocity (PigZombie z) = entityVelocity z
  entityMeta (PigZombie z) = entityMeta z

-- | A Enderman entity
data Enderman = Enderman Insentient (Maybe BlockState) Bool
-- | Trivial instance
instance Mob Enderman where {}
-- | Trivial instance
instance Entity Enderman where
  entityName = "Enderman"
  entityType = 58
  entitySize _ = (0.6,2.9,0.6) --  minecraft:enderman
  entityLocation (Enderman i _ _) = insentientLocation i
  entityVelocity (Enderman i _ _) = insentientVelocity i
  entityMeta (Enderman i mHolding scream) = insentientMeta i ++ [mm mHolding,mm scream]

-- | A CaveSpider entity
newtype CaveSpider = CaveSpider Spider
-- | Trivial instance
instance Mob CaveSpider where {}
-- | Trivial instance
instance Entity CaveSpider where
  entityName = "CaveSpider"
  entityType = 59
  entitySize _ = (0.7,0.5,0.7) --  minecraft:cave_spider
  entityLocation (CaveSpider s) = entityLocation s
  entityVelocity (CaveSpider s) = entityVelocity s
  entityMeta (CaveSpider s) = entityMeta s

-- | A Silverfish entity
newtype Silverfish = Silverfish Insentient
-- | Trivial instance
instance Mob Silverfish where {}
-- | Trivial instance
instance Entity Silverfish where
  entityName = "Silverfish"
  entityType = 60
  entitySize _ = (0.4,0.3,0.4) --  minecraft:silverfish
  entityLocation (Silverfish i) = insentientLocation i
  entityVelocity (Silverfish i) = insentientVelocity i
  entityMeta (Silverfish i) = insentientMeta i

-- | A Blaze entity
data Blaze = Blaze Insentient Bool
-- | Trivial instance
instance Mob Blaze where {}
-- | Trivial instance
instance Entity Blaze where
  entityName = "Blaze"
  entityType = 61
  entitySize _ = (0.6,1.8,0.6) --  minecraft:blaze
  entityLocation (Blaze i _) = insentientLocation i
  entityVelocity (Blaze i _) = insentientVelocity i
  entityMeta (Blaze i onFire) = insentientMeta i ++ [mm onFire]

-- | A LavaSlime entity
newtype LavaSlime = LavaSlime Slime
-- | Trivial instance
instance Mob LavaSlime where {}
-- | Trivial instance
instance Entity LavaSlime where
  entityName = "LavaSlime"
  entityType = 62
  entitySize (LavaSlime s) = entitySize s
  entityLocation (LavaSlime s) = entityLocation s
  entityVelocity (LavaSlime s) = entityVelocity s
  entityMeta (LavaSlime s) = entityMeta s

-- | A EnderDragon entity
data EnderDragon = EnderDragon Insentient VarInt
-- | Trivial instance
instance Mob EnderDragon where {}
-- | Trivial instance
instance Entity EnderDragon where
  entityName = "EnderDragon"
  entityType = 63
  entitySize _ = (16.0,8.0,16.0) --  minecraft:ender_dragon
  entityLocation (EnderDragon i _) = insentientLocation i
  entityVelocity (EnderDragon i _) = insentientVelocity i
  entityMeta (EnderDragon i phase) = insentientMeta i ++ [mm phase]

-- | A WitherBoss entity
data WitherBoss = WitherBoss Insentient VarInt VarInt VarInt VarInt
-- | Trivial instance
instance Mob WitherBoss where {}
-- | Trivial instance
instance Entity WitherBoss where
  entityName = "WitherBoss"
  entityType = 64
  entitySize _ = (0.9,3.5,0.9) --  minecraft:wither
  entityLocation (WitherBoss i _ _ _ _) = insentientLocation i
  entityVelocity (WitherBoss i _ _ _ _) = insentientVelocity i
  entityMeta (WitherBoss i targetEID leftTargetEID rightTargetEID invulnTime) = insentientMeta i ++ [mm targetEID, mm leftTargetEID, mm rightTargetEID, mm invulnTime]

-- | A Bat entity
data Bat = Bat Insentient Bool
-- | Trivial instance
instance Mob Bat where {}
-- | Trivial instance
instance Entity Bat where
  entityName = "Bat"
  entityType = 65
  entitySize _ = (0.5,0.9,0.5) --  minecraft:bat
  entityLocation (Bat i _) = insentientLocation i
  entityVelocity (Bat i _) = insentientVelocity i
  entityMeta (Bat i hanging) = insentientMeta i ++ [mm hanging]

-- | A Witch entity
data Witch = Witch Insentient Bool
-- | Trivial instance
instance Mob Witch where {}
-- | Trivial instance
instance Entity Witch where
  entityName = "Witch"
  entityType = 66
  entitySize _ = (0.6,1.95,0.6) --  minecraft:witch
  entityLocation (Witch i _) = insentientLocation i
  entityVelocity (Witch i _) = insentientVelocity i
  entityMeta (Witch i agro) = insentientMeta i ++ [mm agro]

-- | A Endermite entity
newtype Endermite = Endermite Insentient
-- | Trivial instance
instance Mob Endermite where {}
-- | Trivial instance
instance Entity Endermite where
  entityName = "Endermite"
  entityType = 67
  entitySize _ = (0.4,0.3,0.4) --  minecraft:endermite
  entityLocation (Endermite i) = insentientLocation i
  entityVelocity (Endermite i) = insentientVelocity i
  entityMeta (Endermite i) = insentientMeta i

-- | A Guardian entity
data Guardian = Guardian Insentient Bool VarInt
-- | Trivial instance
instance Mob Guardian where {}
-- | Trivial instance
instance Entity Guardian where
  entityName = "Guardian"
  entityType = 68
  entitySize _ = (0.85,0.85,0.85) --  minecraft:guardian
  entityLocation (Guardian i _ _) = insentientLocation i
  entityVelocity (Guardian i _ _) = insentientVelocity i
  entityMeta (Guardian i retractSpikes targetEID) = insentientMeta i ++ [mm retractSpikes,mm targetEID]

-- | A Shulker entity
data Shulker = Shulker Insentient BlockFace (ProtocolOptional BlockCoord) Word8 Word8
-- | Trivial instance
instance Mob Shulker where {}
-- | Trivial instance
instance Entity Shulker where
  entityName = "Shulker"
  entityType = 69
  entitySize _ = (1,1,1) --  minecraft:shulker
  entityLocation (Shulker i _ _ _ _) = insentientLocation i
  entityVelocity (Shulker i _ _ _ _) = insentientVelocity i
  entityMeta (Shulker i facing attachmentPos shield color) = insentientMeta i ++ [mm facing,mm attachmentPos,mm shield,mm color]

-- | A Pig entity
data Pig = Pig Insentient Bool Bool VarInt
-- | Trivial instance
instance Mob Pig where {}
-- | Trivial instance
instance Entity Pig where
  entityName = "Pig"
  entityType = 90
  entitySize _ = (0.9,0.9,0.9) --  minecraft:pig
  entityLocation (Pig i _ _ _) = insentientLocation i
  entityVelocity (Pig i _ _ _) = insentientVelocity i
  entityMeta (Pig i baby saddled boostTime) = insentientMeta i ++ [mm baby,mm saddled,mm boostTime]

-- | A Sheep entity
data Sheep = Sheep Insentient Bool Word8
-- | Trivial instance
instance Mob Sheep where {}
-- | Trivial instance
instance Entity Sheep where
  entityName = "Sheep"
  entityType = 91
  entitySize _ = (0.9,1.3,0.9) --  minecraft:sheep
  entityLocation (Sheep i _ _) = insentientLocation i
  entityVelocity (Sheep i _ _) = insentientVelocity i
  entityMeta (Sheep i baby sheepBM) = insentientMeta i ++ [mm baby,mm sheepBM]

-- WTF? This isn't listed at all on the wiki lol
-- Here we just assume it's an `Ageable` with no extra metadata
-- | A Cow entity
data Cow = Cow Insentient Bool
-- | Trivial instance
instance Mob Cow where {}
-- | Trivial instance
instance Entity Cow where
  entityName = "Cow"
  entityType = 92
  entitySize _ = (0.9,1.4,0.9) --  minecraft:cow
  entityLocation (Cow i _) = insentientLocation i
  entityVelocity (Cow i _) = insentientVelocity i
  entityMeta (Cow i baby) = insentientMeta i ++ [mm baby]

-- | A Chicken entity
data Chicken = Chicken Insentient Bool
-- | Trivial instance
instance Mob Chicken where {}
-- | Trivial instance
instance Entity Chicken where
  entityName = "Chicken"
  entityType = 93
  entitySize _ = (0.4,0.7,0.4) --  minecraft:chicken
  entityLocation (Chicken i _) = insentientLocation i
  entityVelocity (Chicken i _) = insentientVelocity i
  entityMeta (Chicken i baby) = insentientMeta i ++ [mm baby]

-- | A Squid entity
newtype Squid = Squid Insentient
-- | Trivial instance
instance Mob Squid where {}
-- | Trivial instance
instance Entity Squid where
  entityName = "Squid"
  entityType = 94
  entitySize _ = (0.8,0.8,0.8) --  minecraft:squid
  entityLocation (Squid i) = insentientLocation i
  entityVelocity (Squid i) = insentientVelocity i
  entityMeta (Squid i) = insentientMeta i

-- | A Wolf entity
data Wolf = Wolf Insentient Word8 (ProtocolOptional UUID) Float Bool VarInt
-- | Trivial instance
instance Mob Wolf where {}
-- | Trivial instance
instance Entity Wolf where
  entityName = "Wolf"
  entityType = 95
  entitySize _ = (0.6,0.85,0.6) --  minecraft:wolf
  entityLocation (Wolf i _ _ _ _ _) = insentientLocation i
  entityVelocity (Wolf i _ _ _ _ _) = insentientVelocity i
  entityMeta (Wolf i tameBM ownerUUID hpTail begging collar) = insentientMeta i ++ [mm tameBM,mm ownerUUID,mm hpTail,mm begging,mm collar]

-- | A Mooshroom entity
newtype Mooshroom = Mooshroom Cow
-- | Trivial instance
instance Mob Mooshroom where {}
-- | Trivial instance
instance Entity Mooshroom where
  entityName = "MushroomCow "
  entityType = 96
  entitySize _ = (0.9,1.4,0.9)
  entityLocation (Mooshroom c) = entityLocation c
  entityVelocity (Mooshroom c) = entityVelocity c
  entityMeta (Mooshroom c) = entityMeta c
