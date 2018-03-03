{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
module Civskell.Data.Item where
import Data.Semigroup
import Civskell.Data.Protocol
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics
import Data.SuchThat
import Data.NBT
import Data.Word
import Control.Lens
-- | A @'Window' w@ is a pair of a @w@, representing the data for that window, and an intretation of @w@ as a GUI window.
data Window w = Window (WindowDescriptor w) w

-- | A description of how to interpret a @w@ as a @'Window'@.
data WindowDescriptor w = WindowDescriptor
  {windowName :: Text
  -- ^ The Notchian english name for this kind of window. Ex: @\"Chest"@
  ,windowIdentifier :: String
  -- ^ The Notchian identifier for this kind of window. Ex: @\"minecraft:chest"@
  ,slotCount :: w -> Short
  -- ^ The number of slots in this kind of window. Ex: @'const' 27@
  --,onWindowClick :: WindowClickCallback w
  -- ^ The callback to be invoked when they click on a window of this kind
  }
-- We can do this generically because it depends only on information exposed in the WindowDescriptor.
instance Show (Window w) where
  show (Window desc _w) = T.unpack (windowName desc) <> " (" <> windowIdentifier desc <> ")"

-- | A @'Slot'@ is a possibly empty @'SlotData'@.
-- It has a serialized normal form, see @'WireSlot'@ and @'toWireSlot'@.
newtype Slot i = Slot (Maybe (SlotData i)) deriving (Eq,Generic)

instance Rewrapped (Slot i) t
instance Wrapped (Slot i) where
  type Unwrapped (Slot i) = Maybe (SlotData i)

-- We can do this generically, but we can't give specific information from the type variable
-- because the ItemDescriptor is under the Maybe.
-- UndecidableInstance
instance Show (SlotData i) => Show (Slot i) where
  show (Slot (Just dat)) = "{" ++ show dat ++ "}"
  show (Slot Nothing) = "{}"

-- | A @'SlotData'@ is an @'Item'@ with with an amount.
-- It represents the contents of a single, non-empty item slot.
data SlotData i = SlotData (Item i) Word8 deriving Eq

-- Pretty print a slot's data. Perhaps we should let `Item`s provide a fancy name for this instance to use.
instance Show (SlotData i) where
  show (SlotData i count) = show count ++ " of " ++ show i

-- | Every @'Slot'@ can be serialized for free because the serialization depends only 
-- on the @'itemId'@, @'itemMeta'@, and @'itemNBT'@. We *can not* deserialize from a
-- @'WireSlot'@ for free because that requires a mapping from item ids to items. See
-- @'SupportedItems'@ and @'parseItemFromSet'@ for more information.
toWireSlot :: Slot i -> WireSlot
toWireSlot (Slot Nothing) = WireSlot Nothing
toWireSlot (Slot (Just (SlotData (Item d i) c))) = WireSlot . Just $ (itemId d, c, itemMeta d i, itemNBT d i)

-- | A mapping from @'ItemId'@s to ways to construct an item of that type from
-- metadata and NBT alone (see @'ItemBuilder'@). This is sort of like a set of parsers.
type SupportedItems = Map.Map ItemId (ForAny ItemBuilder)

-- | A way to construct an @'Item'@ from metadata and NBT. These will be paired
-- up with @'ItemId'@s from the @'Map.Map'@ in @'SupportedItems'@.
newtype ItemBuilder i = ItemBuilder {buildItem :: Short -> Maybe NBT -> Item i}

-- | Given a set of items that are supported, and a @'WireSlot'@,
-- parse out a type-enriched @'Slot'@ of some type, or fail because we don't
-- support the provided item id.
parseItemFromSet :: SupportedItems -> WireSlot -> Maybe (ForAny Slot)
parseItemFromSet _ (WireSlot Nothing) = Just (ambiguate $ Slot Nothing)
parseItemFromSet si (WireSlot (Just (iId,count,meta,mNBT))) = case Map.lookup iId si of
  Just (SuchThat b) -> Just . ambiguate . Slot . Just $ SlotData (buildItem b meta mNBT) count
  Nothing -> Nothing

-- | An @'Item'@ is an @i@, together with a description of how to interpret it as a Minecraft item.
data Item i = Item (ItemDescriptor i) i

-- This instances says that items with the same id, meta and NBT, are literally equal, which isn't always true
instance Eq (Item i) where
  (Item da a) == (Item db b) = itemId da == itemId db && itemIdentifier da == itemIdentifier db && itemMeta da a == itemMeta db b && itemNBT da a == itemNBT db b

instance Show (Item i) where
  show (Item desc i) = "[" ++ show (itemId desc) ++ ":" ++ show (itemMeta desc i) ++ "]" ++ n (itemNBT desc i)
    where
      -- Only display the NBT if it is actually there
      n Nothing = ""
      n (Just nbt) = " with tags: " ++ show nbt

-- | A description of how to interpret an @i@ as a Minecraft item.
data ItemDescriptor i = ItemDescriptor
  -- TODO: Type level this when we have dependent types
  {itemId :: ItemId
  -- ^ The @'ItemId'@ of this item. Ex: @280@
  ,itemIdentifier :: String
  -- ^ The Notchian identifier of this item. Ex: @\"minecraft:stick"@
  ,itemMeta :: i -> Short
  -- ^ The metadata associated with this particular item.
  ,itemNBT :: i -> Maybe NBT
  -- ^ The NBT of this particular item, if present.
  --,onItemUse :: ItemUseCallback i
  -- ^ The callback for when a player right-clicks with this item.
  }



