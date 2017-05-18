module Civskell.Data.Slot 
  (Slot
  ,
  ) where

newtype Slot = Slot (Maybe SlotData)
data SlotData = SlotData (Some Item) Word8
type Inventory = Map Short Slot

getSlot :: Short -> Inventory -> Slot
getSlot = Map.lookup

setSlot :: Short -> Slot -> Inventory -> Inventory
setSlot i Nothing = Map.delete i
setSlot i (Just x) = Map.insert i x

slot :: Item i => i -> Word8 -> Slot
slot i c = Slot . Just $ SlotData (some i) c

instance Eq Slot where
  (Slot (SuchThat (Identity (a :: at))) ac) == (Slot (SuchThat (Identity (b :: bt))) bc) = itemId @at == itemId @bt && itemMeta a == itemMeta b && itemNBT a == itemNBT b && ac == bc

slotAmount :: Slot -> Word8
slotAmount (Slot (Just (SlotData _ c))) = c
slotAmount (Slot Nothing) = 0

-- Remove as many items as possible, up to count, return amount removed
removeCount :: Word8 -> Slot -> (Word8,Slot)
removeCount _ (Slot Nothing) = (0,Slot Nothing)
removeCount c (Slot (Just (SlotData i count))) = if c < count then (c,Slot . Just $ SlotData i (count - c)) else (count,Slot Nothing)

-- Count is how many to put into first stack from second
splitStack :: Word8 -> Slot -> (Slot,Slot)
splitStack _ (Slot Nothing) = (Slot Nothing,Slot Nothing)
splitStack c s@(Slot (Just (SlotData i _))) = (firstStack,secondStack)
  where
    firstStack = if amtFirstStack == 0 then Slot Nothing else Slot . Just $ SlotData i amtFirstStack
    (amtFirstStack,secondStack) = removeCount c s

instance Show SlotData where
  show (SlotData (SuchThat (Identity (i :: it))) count) = "{" ++ show count ++ " of [" ++ show (itemId @it) ++ ":" ++ show (itemMeta i) ++ "]" ++ n (itemNBT i) ++ "}"
    where
      -- Only display the NBT if it is actually there
      n Nothing = ""
      n (Just nbt) = " with tags: " ++ show nbt

instance Serial Slot where
  serialize (Slot Nothing) = serialize (-1 :: Short)
  serialize (Slot (Just (SlotData (SuchThat (Identity (i :: it))) count))) = serialize (itemId @it) >> serialize count >> serialize (itemMeta i) >> (case itemNBT i of {Just nbt -> serialize (NBT "" nbt);Nothing -> putWord8 0x00})


