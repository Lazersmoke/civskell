{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
-- ^^^ BEHOLD: THE 20 HORSEMEN OF THE HASKPOCALYPSE ^^^ --

-- TODO: This export list is pretty scary. We should make tiered modules or something
module Civskell.Data.Types
  -- Constants
  (airChunk,allCoords,protocolVersion,legacyHandshakePacketConstant
  -- Helper functions
  ,blockInChunk,blockToChunk,blockToRelative,blockOnSide,showPacket,jsonyText,withLength,withListLength,indentedHex,serOpt,comb
  -- Type synonyms
  ,EncryptionCouplet,PerformsIO,Short
  -- Blocks
  ,BlockBreak(..),BlockLocation(..),BlockCoord,BlockOffset,BlockFace(..),CardinalDirection(..),BlockState(..)
  -- Block class
  ,Block(..),some,Air(..),showBlock,serializeBlock
  ,Item(..)
  -- Chunks
  ,ChunkCoord(..),ChunkSection(..)
  -- Packet Information
  ,AnimationAction(..),PlayerListAction(..),PlayerListActionType(..),EntityInteraction(..)
  ,ClientStatusAction(..),GameStateChange(..),InventoryClickMode(..),PlayerDigAction(..)
  ,PlayerEntityAction(..),AuthPacket(..),AuthProperty(..)
  -- Id's and newtypes
  ,VarInt(..),EntityId(..),PlayerId,WindowId(..),KeepAliveId,TPConfirmId,TransactionId
  -- Aux Data Types
  ,PlayerData(..),UUID(..)
  -- Slot
  ,Slot(..),slotAmount,removeCount,splitStack,slot
  ,Inventory,getSlot,setSlot
  -- Aux Enums
  ,Difficulty(..),Dimension(..),Gamemode(..),Hand(..),MoveMode(..),AbilityFlags(..),ServerState(..),Side(..),Window(..)
  -- Player
  ,HasPlayer,Player(..)
  -- World
  ,HasWorld,WorldData(..),World(..)
  -- Logging
  ,Logs,Logging(..),LogLevel(..)
  -- Network
  ,Networks,Networking(..)
  -- Packeting
  ,SendsPackets,Packeting(..)
  -- Packet
  ,Packet(..),ClientPacket,HandledPacket(..),OutboundPacket(..),InboundPacket(..)
  -- Configuration
  ,Configured,Configuration(..),defaultConfiguration,forkConfig
  -- Entity
  ,Entity(..)
  ,EntityMetaType(..)
  ,EntityMeta(..)
  ,EntityMetadata
  ,EntityLocation(..)
  ,EntityVelocity(..)
  ,Mob
  ,Object(..)
  ,Serialize(..)
  ) where

import Control.Concurrent.STM
import Data.Functor.Identity
import Control.Eff
import Control.Eff.Reader
import Crypto.Cipher.AES (AES128)
import Data.Aeson hiding (Object)
import Data.Aeson.Types hiding (Parser,Object)
import Data.Attoparsec.ByteString (Parser)
import Data.Bits
import Data.Bits (Bits)
import Data.Int (Int16,Int32,Int64)
import Data.List (intercalate)
import Data.List (unfoldr,foldl')
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import Data.NBT
import Data.Semigroup ((<>))
import Data.SuchThat
import Data.Word (Word64,Word8)
import Hexdump (prettyHex)
import Numeric (readHex)
import Numeric (showHex)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Aeson
import qualified Data.Binary.BitBuilder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8
import qualified Data.Map.Lazy as Map
import qualified Data.Serialize as Ser
import qualified Data.Set as Set
import qualified Data.Text as Text

-- The current Minecraft Protocol Version number. This should be replaced with a
-- configuration option, or even multiversion support in the future, but right
-- now we only support 316, which is 1.11.2 Vanilla.
protocolVersion :: Integer
protocolVersion = 316

-- Legacy Handshake packet constant. This is what we send when we get a legacy
-- ping from a client.
legacyHandshakePongConstant :: BS.ByteString
legacyHandshakePongConstant = BS.pack [0xFF,0x00,0x1b,0x00,0xa7
  ,0x00,0x31 -- 1
  ,0x00,0x00 -- Seperator
  ,0x00,0x33 -- 3 -- Protocol Version number. TODO: Update this with protocolVersion
  ,0x00,0x31 -- 1
  ,0x00,0x36 -- 6
  ,0x00,0x00 -- Seperator
  ,0x00,0x31 -- 1 -- MC Version
  ,0x00,0x2e -- .
  ,0x00,0x31 -- 1
  ,0x00,0x31 -- 1
  ,0x00,0x2e -- .
  ,0x00,0x32 -- 2
  ,0x00,0x00 -- Seperator
  ,0x00,0x43 -- C -- Server Name
  ,0x00,0x69 -- i
  ,0x00,0x76 -- v
  ,0x00,0x73 -- s
  ,0x00,0x6b -- k
  ,0x00,0x65 -- e
  ,0x00,0x6c -- l
  ,0x00,0x6c -- l
  ,0x00,0x00 -- Seperator
  ,0x00,0x00 -- 0
  ,0x00,0x00 -- Seperator
  ,0x00,0x00 -- 0
  ]

-- First part of the legacy client ping packet
legacyHandshakePingConstant :: BS.ByteString
legacyHandshakePingConstant = BS.pack
  [0xFE -- SLP PktId
  ,0x01 -- SLP "payload"
  ,0xFA -- Plugin Message
  ,0x00,0x0B -- Length of "MC|PingHost"
  ,0x00,0x4D -- M
  ,0x00,0x43 -- C
  ,0x00,0x7C -- |
  ,0x00,0x50 -- P
  ,0x00,0x69 -- i
  ,0x00,0x6E -- n
  ,0x00,0x67 -- g
  ,0x00,0x48 -- H
  ,0x00,0x6F -- o
  ,0x00,0x73 -- s
  ,0x00,0x74 -- t
  ]

-- A LegacyString is a String that is used in a Legacy Handshake packet
type LegacyString = BS.ByteString

{-
 - This instance is deprecated in favor of just storing the 0x00's in with all
 - the other data. We only use LegacyString like twice, so this is reasonable.
instance Serial LegacyString where
  serialize (LegacyString bs) = BS.cons 0x00 (intersperse 0x00 bs)
  deserialize = error "Undefined: deserialize @LegacyString"
-}

-- A Minecraft "Short" is a Haskell Int16
type Short = Int16

-- A Nibble is a Word8 that we promise (not enforced!!!) to only use the 4 least
-- significant bits.
-- TODO: Investigate `(Bool,Bool,Bool,Bool)` or `Vect 4 Bool` as alternative.
type Nibble = Word8

-- An EncrptionCouplet holds the current internal state of the encryption
-- mechanism, including both directions' shift buffers. (Cipher,Enc,Dec)
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- Verbify this to match all our other type synonyms like `HasWorld`
type PerformsIO r = Member IO r

-- Removed in favor of using send directly:
-- You make an effect that `PerformsIO` by `send`ing an IO action (`send (putStrLn "meow")`)
-- liftIO :: PerformsIO r => IO a -> Eff r a
-- liftIO = send

-- TODO: Investigate using a library to provide this type for seperation of concerns
-- There is no native Word128 type, so we role our own here.
newtype UUID = UUID (Word64,Word64)

-- This show instance should match the standard Minecraft display format.
instance Show UUID where
  show (UUID (ua,ub)) = reformat $ showHex ua (showHex ub "")
    where
      -- Add a hyphen at an index
      ins i s = let (a,b) = splitAt i s in a ++ "-" ++ b
      -- Reformat the uuid to what the client expects
      reformat = ins 8 . ins 12 . ins 16 . ins 20

-- A UUID is pretending to be a Word128, so just smoosh all the bits together
instance Serialize UUID where
  serialize (UUID (a,b)) = serialize a <> serialize b

-- Simple way to inject a text message into a json chat string. No additional
-- formatting or checking is done, just raw text.
jsonyText :: String -> String
jsonyText s = "{\"text\":\"" ++ s ++ "\"}"

-- Used in PlayerInfo to track the breaking stage of a block the player is mining.
data BlockBreak 
  = InProgress Word8 -- The block is still being broken, and has been for this many ticks.
  | DoneBreaking -- The block is done breaking and should be removed, and this entry removed.

-- A block coordinate, not an entity position. Ord is derivied for use in maps,
-- but does not represent any specific concrete ordering, and should not be
-- relied on for storage etc.
data BlockLocation (r :: Relativity)
  = BlockLocation (Int,Int,Int) -- (x,y,z)
  deriving (Eq,Ord)

-- BlockLocations have some weakish dimensionality. `Absolute` means that it is
-- an absolute coord relative to (0,0,0). `Relative` means that it is an offset,
-- often, but not always, relative to a chunk's local (min x,min y = 0, min z)
type BlockCoord = BlockLocation 'Absolute
type BlockOffset = BlockLocation 'Relative

-- Have different Show instances to respect the difference as well.
instance Show BlockCoord where
  show (BlockLocation (x,y,z)) = "(Block)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

instance Show BlockOffset where
  show (BlockLocation (x,y,z)) = "(Offset)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

{- This is a bad instance that took almost an hour to debug. It still isn't fixed, just worked around (it's derived, and we don't rely on its ordering in Map.elems)
 - QuickCheck your instances, kids!
instance Ord BlockCoord where
  compare a@(BlockCoord (xa,ya,za)) b@(BlockCoord (xb,yb,zb)) = if a == b then EQ else if yb > ya then GT else if zb > za then GT else if xb > xa then GT else LT
-}

-- Only absolute coords can be serialized. This uses Minecraft's efficient
-- packing of coords into bits of an Int64 because the Y coord is very limited
-- in range compared to X and Z.
instance Serialize BlockCoord where
  serialize (BlockLocation (x,y,z)) = serialize $
    -- 26 MSB
    (shiftL (u x .&. 0x3FFFFFF) 38) .|.
    -- 12 middle bits
    (shiftL (u y .&. 0xFFF) 26) .|.
    -- 26 LSB
    (u z .&. 0x3FFFFFF)
    where
      u = unsafeCoerce :: Int -> Int64

-- A Chunk Coord is a coordinate *of* a chunk slice, not *in* a chunk slice.
-- TODO: Investigate not using chunks at all, and only serializing to chunks on
-- demand. This can probably be very lazy and very efficient. Maybe even include
-- lazy chunk generation!
newtype ChunkCoord = ChunkCoord (Int,Int,Int) deriving (Eq,Ord)

-- This mirrors the format of `Show BlockLocation`
instance Show ChunkCoord where
  show (ChunkCoord (x,y,z)) = "(Chunk)<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

-- VarInt's max size is an Int32, so we use that to store it. 
-- Note that they are serialized differently from a normal Minecraft Int (which
-- is a normal Haskell Int32), so we need a newtype. All instances are GND
newtype VarInt = VarInt {unVarInt :: Int32} deriving (Show,Bits,Enum,Eq,Integral,Num,Ord,Real)

-- VarInt format in binary is:
--   
--   iddddddd iddddddd ...
-- 
-- Where when `i` is set, there is still another byte to be read. All the `d`s
-- from all the bytes are concatenated without spacing to form the actual number.
instance Serialize VarInt where
  serialize n = if moreAfter
    -- If there are more, set the msb and recurse
    then (0b10000000 .|. writeNow) `BS.cons` serialize (shiftR n 7)
    -- Otherwise, just use this one
    else BS.singleton writeNow
    where
      -- Write first seven bits
      writeNow = (unsafeCoerce :: VarInt -> Word8) $ n .&. 0b1111111
      -- Are there more bytes to add?
      moreAfter = shiftR n 7 /= 0

-- EntityId's are distinguished VarInt's. Note that the `Serial` instance here is
-- GND, not Generics. (although it could be, and it would work the same)
newtype EntityId = EntityId {unEID :: VarInt} deriving (Serial,Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show EntityId where show = (\p -> "Entity Id {" ++ p ++ "}") . show . unEID

-- WindowId's are distinguished Word8's. `Serial` is GND, not Generics.
newtype WindowId = WindowId {unWID :: Word8} deriving (Serial,Bits,Enum,Eq,Integral,Num,Ord,Real)
instance Show WindowId where show = (\p -> "Window Id {" ++ p ++ "}") . show . unWID

-- PlayerId's are *undistinguished* EntityId's because they are not different in
-- any way except the probably represent players instead of mobs. This is made
-- explicit to prevent additional confusion.
type PlayerId = EntityId

-- These are undistinguished because they are used infrequently. 
-- TODO: Investigate if these should actually be newtypes
type TransactionId = Short
type KeepAliveId = VarInt
type TPConfirmId = VarInt

-- Slots are refactored to this on merge:
-- A SlotData is a set of information about a specific non-empty Minecraft Slot.
-- A Slot is a potentially empty Minecraft Slot.

-- Item id, count, damage
data Slot = Slot (Some Item) Word8

-- An inventory is a mapping from Slot numbers to Slot data (or lack thereof).
-- TODO: Reconsider sparsity obligations of Map here, and decide on a better
-- internal representation. Potentially make this abstract after all. There
-- might be a really fitting purely functional data structure to show off here.
type Inventory = Map Short Slot

-- Abstraction methods for Inventory, not really needed tbh.
getSlot :: Short -> Inventory -> Maybe Slot
getSlot = Map.lookup

setSlot :: Short -> Maybe Slot -> Inventory -> Inventory
setSlot i Nothing = Map.delete i
setSlot i (Just x) = Map.insert i x

-- Sidestep existential nonsense when creating items
slot :: Item i => i -> Word8 -> Slot
slot i c = Slot (some i) c

-- This is a rather sketchy instance because itemId uniqueness is not enforced
-- at type level, so we could actually get two different Item instances claiming
-- the same id, meta, and NBT, which would make this `Eq` instance unreliable.
-- In fact, it might be *impossible* to write an `Eq` instance for Slots
-- involving general, open typeclass `Item`s because that would involve deciding
-- equality for a `forall a. c a => p a` sort of thing, which would only be
-- based on the `c` instance.
instance Eq Slot where
  (Slot (SuchThat (Identity (a :: at))) ac) == (Slot (SuchThat (Identity (b :: bt))) bc) = itemId @at == itemId @bt && itemMeta a == itemMeta b && itemNBT a == itemNBT b && ac == bc

-- Extract the item count from a Slot
slotAmount :: Slot -> Word8
slotAmount (Slot _ c) = c

-- Remove as many items as possible, up to count, return amount removed
removeCount :: Word8 -> Maybe Slot -> (Word8,Maybe Slot)
removeCount _ Nothing = (0,Nothing)
removeCount c (Just (Slot i count)) = if c < count then (c,Just $ Slot i (count - c)) else (count,Nothing)

-- Count is how many to put into first stack from second
splitStack :: Word8 -> Maybe Slot -> (Maybe Slot,Maybe Slot)
splitStack _ Nothing = (Nothing,Nothing)
splitStack c s@(Just (Slot i _)) = (firstStack,secondStack)
  where
    firstStack = if amtFirstStack == 0 then Nothing else Just $ Slot i amtFirstStack
    (amtFirstStack,secondStack) = removeCount c s

-- Pretty print a slot. Perhaps we should let `Item`s provide a fancy name for
-- this instance to use.
instance Show Slot where
  show (Slot (SuchThat (Identity (i :: it))) count) = "{" ++ show count ++ " of [" ++ show (itemId @it) ++ ":" ++ show (itemMeta i) ++ "]" ++ n (itemNBT i) ++ "}"
    where
      -- Only display the NBT if it is actually there
      n Nothing = ""
      n (Just nbt) = " with tags: " ++ show nbt

-- Removed on merge
instance Serialize (Maybe Slot) where
  serialize Nothing = serialize (-1 :: Short)
  serialize (Just (Slot (SuchThat (Identity (i :: it))) count)) = serialize (itemId @it) <> serialize count <> serialize (itemMeta i) <> (case itemNBT i of {Just nbt -> serialize (NBT "" nbt);Nothing -> BS.singleton 0x00})

class Window w where
  -- Chest
  windowName :: String
  -- minecraft:chest
  windowIdentifier :: String
  -- 27
  slotCount :: Short
  -- Bool is transaction success
  onWindowClick :: (HasWorld r,SendsPackets r,Logs r,HasPlayer r) => w -> WindowId -> Short -> TransactionId -> InventoryClickMode -> Eff r Bool
  --clientToCivskellSlot :: Short -> Short
  --civskellToClientSlot :: Short -> Short

-- TODO: Reconsider sparsity obligations. This is pretty dumb in terms of
-- invariants we must enforce.
-- Maps coords to non-air blocks. BlockState 0 0 doesn't mean anything. Air just doesn't have anything in the map.
data ChunkSection = ChunkSection (Map BlockOffset (Some Block))

-- Derived instance is really long (it shows all the associations of coords and
-- blocks in a giant list), so we replace it with a short placeholder
instance Show ChunkSection where
  show _ = "<Chunk Section>"

-- TODO: Add paletteing. Right now, we send a full 13 bits for *every block*
-- On second thought, we should rewrite this cancer with the `Data.Bytes` update
instance Serialize ChunkSection where
  serialize (ChunkSection bs) = serialize bitsPerBlock <> palette <> chunkData <> lights <> lights
    where
      lights = LBS.toStrict $ BB.toLazyByteString $ createLights $ merged

      -- First 9 bits are the block id, last 4 are the damage
      bitsPerBlock = 13 :: VarInt

      -- Send 0x00 as the length of the palette since we aren't using it
      palette = BS.singleton 0x00

      -- Notify and crash *right away* if this gets fucked up. This only happens if we have a bad Ord instance.
      -- Note that Map.size is O(1) so we aren't wasting too much time here
      -- Merge the chunk's blocks with air because air blocks don't exist in the chunk normally
      merged :: Map BlockOffset (Some Block)
      merged = if Map.size (Map.union bs airChunk) == 4096 then Map.union bs airChunk else error "Bad Chunksection"

      -- `longChunks` is because Mojang likes to twist their data into weird shapes
      sArray :: BS.ByteString
      sArray = LBS.toStrict . longChunks . BB.toLazyByteString $ sBlockStates merged

      -- This should be the final result, but mojang is weird about chunk sections :S
      sBlockStates :: Map BlockOffset (Some Block) -> BB.BitBuilder
      sBlockStates m = foldl' (\bb bc -> ambiguously (aBlock . runIdentity) (Map.findWithDefault (some Air) bc m) `BB.append` bb) BB.empty allCoords

      -- Annotate the data with its length in Minecraft Longs (should always be a whole number assuming 16^3 blocks/chunk)
      chunkData :: BS.ByteString
      chunkData = serialize (fromIntegral (BS.length sArray) `div` 8 :: VarInt) <> sArray

      -- Right now we `const 15` the blocks, so all the blocks are fullbright
      -- TODO: Add lighting information somewhere or derive it here
      createLights = Map.foldl' (\bb _ -> BB.fromBits 4 (15 :: Word8) `BB.append` bb) BB.empty

      -- Mojang felt like packing chunks into arrays of longs lol
      longChunks :: LBS.ByteString -> LBS.ByteString
      longChunks unchunked = LBS.concat . reverse $ bsChunksOf 8 unchunked

      -- Helper function for chunking up the BS
      bsChunksOf :: Int64 -> LBS.ByteString -> [LBS.ByteString]
      bsChunksOf x = unfoldr (\a -> if not (LBS.null a) then Just (LBS.splitAt x a) else Nothing)

      -- Serialize a single block: 9 bits bid, 4 bits dmg
      aBlock :: Block b => b -> BB.BitBuilder
      aBlock (b :: bt) = BB.fromBits 9 (blockId @bt) `BB.append` BB.fromBits 4 (blockMeta b)

-- Auth infrastructure is scary, I hope it doesn't break ever :3
-- Parsed form of the result of authenticating a player with Mojang
data AuthPacket = AuthPacket UUID String [AuthProperty]

-- FromJSON instances parse JSON into AuthPackets
instance FromJSON AuthPacket where
  parseJSON (Data.Aeson.Object o) = AuthPacket <$> o .: "id" <*> o .: "name" <*> (o .: "properties")
  parseJSON x = typeMismatch "AuthPacket" x

instance FromJSON UUID where
  parseJSON (String s) = return $ (\i -> UUID (fromInteger $ i .&. 0xFFFFFFFFFFFFFFFF,fromInteger $ shiftR i 8)) . fst . head . readHex . Text.unpack $ s
  parseJSON x = typeMismatch "UUID" x

-- Helper type to support the arbitrary properties in auth packets
data AuthProperty = AuthProperty String String (Maybe String)

instance FromJSON AuthProperty where
  parseJSON (Data.Aeson.Object o) = AuthProperty <$> o .: "name" <*> o .: "value" <*> o .:? "signature"
  parseJSON x = typeMismatch "AuthProperty" x

-- This used to be used in PacketSide as a kind
data Side = Server | Client

-- Kind for BlockOffset vs BlockCoord
data Relativity = Relative | Absolute

-- These are used for to select the correct parser (packet id 0 is reused in all four modes)
data ServerState = Handshaking | Playing | LoggingIn | Status

-- Minecraft Notchian Enums
data Gamemode = Survival | Creative {- | Adventure | Spectator -} deriving Show

instance Serialize Gamemode where
  serialize Survival = BS.singleton 0x00
  serialize Creative = BS.singleton 0x01
  -- serialize Adventure = BS.singleton 0x02
  -- serialize Spectator = BS.singleton 0x03

data Difficulty = Peaceful | Easy | Normal | Hard deriving Show

instance Serialize Difficulty where
  serialize Peaceful = BS.singleton 0x00
  serialize Easy = BS.singleton 0x01
  serialize Normal = BS.singleton 0x02
  serialize Hard = BS.singleton 0x03

data Dimension = Nether | Overworld | TheEnd deriving Show

instance Serialize Dimension where
  serialize Nether = serialize (-1 :: Int32)
  serialize Overworld = serialize (0 :: Int32)
  serialize TheEnd = serialize (1 :: Int32)

data Hand = MainHand | OffHand deriving (Show,Eq)

data BlockFace = Bottom | Top | SideFace CardinalDirection deriving Show

instance Enum BlockFace where
  toEnum = \case
    0 -> Bottom
    1 -> Top
    n -> SideFace (toEnum n)
  fromEnum = \case
    Bottom -> 0
    Top -> 1
    SideFace f -> fromEnum f

data CardinalDirection = North | South | West | East deriving Show

instance Enum CardinalDirection where
  toEnum = \case
    2 -> North
    3 -> South
    4 -> West
    5 -> East
    _ -> error "toEnum CardinalDirection"
  fromEnum = \case
    North -> 2
    South -> 3
    West -> 4
    East -> 5

-- Used in PlayerData
data MoveMode = Sprinting | Sneaking | Walking | Gliding | Flying

-- Packet Helper data types
-- Used in Client.Animation; Server.Animation just uses hand
data AnimationAction = SwingHand Hand | Critical Bool | TakeDamage | LeaveBedAnimation deriving Show

instance Serialize AnimationAction where
  serialize (SwingHand MainHand) = BS.singleton 0x00
  serialize TakeDamage = BS.singleton 0x01
  serialize LeaveBedAnimation = BS.singleton 0x02
  serialize (SwingHand OffHand) = BS.singleton 0x03
  serialize (Critical False) = BS.singleton 0x04
  serialize (Critical True) = BS.singleton 0x05

-- Used in Server.UseEntity
data EntityInteraction = Attack | Interact Hand | InteractAt (Float,Float,Float) Hand deriving Show

-- Helper kind for PlayerListAction
data PlayerListActionType = AddPlayer | UpdateGamemode | UpdateLatency | UpdateName | RemovePlayer

-- Used in Client.PlayerListItem
data PlayerListAction (a :: PlayerListActionType) where
  PlayerListAdd :: String -> [AuthProperty] -> Gamemode -> VarInt -> (Maybe String) -> PlayerListAction 'AddPlayer
  PlayerListGamemode :: Gamemode -> PlayerListAction 'UpdateGamemode
  PlayerListLatency :: VarInt -> PlayerListAction 'UpdateLatency
  PlayerListName :: (Maybe String) -> PlayerListAction 'UpdateName
  PlayerListRemove :: PlayerListAction 'RemovePlayer

instance Serialize (UUID,PlayerListAction a) where
  serialize (u,p) = serialize u <> serialize p

instance Serialize (PlayerListAction a) where
  serialize (PlayerListAdd name props gm ping mDispName) = serialize name <> serialize ((fromIntegral $ length props) :: VarInt) <> BS.concat (map serProp props) <> serialize gm <> serialize ping <> serOpt mDispName
    where
      serProp (AuthProperty n value mSigned) = serialize n <> serialize value <> serOpt mSigned
  serialize (PlayerListGamemode gm) = serialize gm
  serialize (PlayerListLatency ping) = serialize ping
  serialize (PlayerListName mDisp) = serOpt mDisp
  serialize PlayerListRemove = BS.empty

-- Used in Server.PlayerDigging
data PlayerDigAction = StartDig BlockCoord BlockFace | StopDig BlockCoord BlockFace | EndDig BlockCoord BlockFace | DropItem Bool | ShootArrowOrFinishEating | SwapHands

-- Used in Server.EntityAction
data PlayerEntityAction = Sneak Bool | Sprint Bool | HorseJump Bool VarInt | LeaveBed | ElytraFly | HorseInventory

-- Used in Server.ClickWindow
data InventoryClickMode = NormalClick Bool | ShiftClick Bool | NumberKey Word8 | MiddleClick | ItemDropOut Bool | PaintingMode Word8 | DoubleClick deriving Show

-- Used in Server. and Client.PlayerAbilities
-- Invuln, Flying, Allow Flying, Creative
data AbilityFlags = AbilityFlags Bool Bool Bool Bool

instance Serialize AbilityFlags where
  serialize (AbilityFlags i f af c) = serialize $ u i 0 .|. u f 1 .|. u af 2 .|. u c 3
    where
      u b n = shiftL (unsafeCoerce b :: Word8) n

-- Used in Client.ChangeGameState
data GameStateChange = InvalidBed | Raining Bool | ChangeGamemode Gamemode | ExitTheEnd Bool | DemoMessage | ArrowHitOtherPlayer | FadeValue Float | FadeTime Float | ElderGuardian

-- Used in Server.ClientAction
data ClientStatusAction = PerformRespawn | RequestStats | OpenInventory deriving Show

-- Block Coord helper functions
-- Get the chunk an absolute coord is in
blockToChunk :: BlockCoord -> ChunkCoord
blockToChunk (BlockLocation (x,y,z)) = ChunkCoord (x `div` 16,y `div` 16,z `div` 16)

-- Get the relative location inside its chunk of an absolute block coord
blockToRelative :: BlockCoord -> BlockOffset
blockToRelative (BlockLocation (x,y,z)) = BlockLocation (f x,f y,f z)
  where
    -- We don't use negative relative coords in negative chunks
    f n = if mod n 16 < 0 then 16 + mod n 16 else mod n 16

-- Get the block at a location in a chunk
blockInChunk :: BlockOffset -> ChunkSection -> Some Block
blockInChunk b (ChunkSection m) = fromMaybe (some Air) (Map.lookup b m)

-- Get the block coord adjacent to a given coord on a given side
blockOnSide :: BlockCoord -> BlockFace -> BlockCoord
blockOnSide (BlockLocation (x,y,z)) Bottom = BlockLocation (x,y-1,z)
blockOnSide (BlockLocation (x,y,z)) Top = BlockLocation (x,y+1,z)
blockOnSide (BlockLocation (x,y,z)) (SideFace North) = BlockLocation (x,y,z-1)
blockOnSide (BlockLocation (x,y,z)) (SideFace South) = BlockLocation (x,y,z+1)
blockOnSide (BlockLocation (x,y,z)) (SideFace West) = BlockLocation (x-1,y,z)
blockOnSide (BlockLocation (x,y,z)) (SideFace East) = BlockLocation (x+1,y,z)

some :: c a => a -> Some c
some = ambiguate . Identity

class Block b where
  -- 1
  blockId :: Short
  -- "minecraft:stone"
  blockIdentifier :: String
  -- Stone -> 0; Granite -> 1
  blockMeta :: b -> Nibble
  blockMeta _ = 0
  -- By default, blocks without meta get 0
  --blockMeta _ = 0
  -- Name in notchian english "Stone" or "Granite"
  blockName :: b -> String
  -- Some blocks do something when clicked
  onClick :: forall r. (HasWorld r,HasPlayer r,SendsPackets r,Logs r) => Maybe (b -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Eff r ())
  onClick = Nothing
  droppedItem :: Maybe (Some Item)

{-
block :: Short -> String -> String -> b -> Block b
block bid ident dat = Block
  {blockId = bid
  ,blockIdentifier = ident
  ,blockData = dat
  ,blockMeta = const 0
  ,blockName = const ident
  ,onClick = Nothing
  }
-}

serializeBlock :: Block b => b -> BS.ByteString
serializeBlock (b :: bt) = serialize $ shiftL (u (blockId @bt)) 4 .|. (v (blockMeta b) .&. 0x0f)
  where
    u = unsafeCoerce :: Short -> VarInt
    v = unsafeCoerce :: Nibble -> VarInt

showBlock :: Block b => b -> String
showBlock (b :: bt) = "Block [" ++ show (blockId @bt) ++ ":" ++ show (blockMeta b) ++ "]"

data Air = Air

instance Block Air where
  blockIdentifier = "minecraft:air"
  blockName _ = "Air"
  blockId = 0

--class Block t => TileEntity t where
  --tileEntityNBT :: t -> NBT

class Item i where
  -- TODO: Type level this when we have dependent types
  itemId :: Short
  itemIdentifier :: String
  itemMeta :: i -> Short
  itemMeta _ = 0
  --itemPlaced :: i -> Maybe (Some Block)
  --itemPlaced _ = Nothing
  itemNBT :: i -> Maybe (NbtContents)
  itemNBT _ = Nothing
  -- TODO: param Slot i
  parseItem :: Parser (Maybe Slot)
  -- Some items do something when right clicked
  onItemUse :: forall r. (HasWorld r,HasPlayer r,SendsPackets r,Logs r) => Maybe (i -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Eff r ())
  onItemUse = Nothing

-- The state of a block in the world (Block id, damage)
data BlockState = BlockState Short Word8 deriving Eq

-- All the relative coords within a chunk
allCoords :: [BlockOffset]
allCoords = [BlockLocation (x,y,z) | y <- [0..15], z <- [0..15], x <- [0..15]]

-- A chunk full of air (used in chunk serialization to fill in gaps in the Map
airChunk :: Map BlockOffset (Some Block)
airChunk = Map.fromList [(BlockLocation (x,y,z),some Air) | x <- [0..15], z <- [0..15], y <- [0..15]]

-- Helper function to format Packets during logging
-- Takes a packet name and a list of properties (name value pairs). Empty values only show the name
formatPacket :: String -> [(String,String)] -> String
formatPacket n [] = "{" ++ n ++ "}"
formatPacket n xs = flip (++) "]" . (++) ("{" ++ n ++ "} [") . intercalate " | " . map (\(name,val) -> if val == "" then name else name ++ ": " ++ val) $ xs

-- Stores all the relevant information about a player
-- TODO: Extensibility?
data PlayerData = PlayerData
  {teleportConfirmationQue :: Set.Set VarInt
  ,nextTid :: VarInt
  ,keepAliveQue :: Set.Set VarInt
  ,nextKid :: VarInt
  ,nextWid :: WindowId
  -- Map of WindowId's to that window's metadata (including accessor effects)
  ,windows :: Map WindowId (Some Window)
  ,failedTransactions :: Set.Set (WindowId,TransactionId)
  ,holdingSlot :: Short
  ,playerPosition :: (Double,Double,Double)
  ,viewAngle :: (Float,Float)
  ,gameMode :: Gamemode
  -- player inventory is window 0
  ,playerInventory :: Inventory
  ,diggingBlocks :: Map BlockCoord BlockBreak
  ,moveMode :: MoveMode
  ,playerState :: ServerState
  ,clientUsername :: String
  ,clientBrand :: String
  ,clientUUID :: UUID
  ,playerId :: PlayerId
  ,packetQueue :: TQueue (ForAny OutboundPacket)
  }

-- Having transactional access to a single Player's data is an effect
type HasPlayer r = Member Player r

data Player a where
  -- Do arbitrary STM given a TVar. TODO: can't they just do arbitrary STM in general if they happen upon a TVar other than the one we provide?
  UsingPlayerData :: (TVar PlayerData -> STM a) -> Player a
  -- Register a player with the World, and return their PId
  RegisterPlayer :: Player PlayerId
  -- Commute a Player effect out of a stack. This is like an "unjoin" in a way
  ForkPlayer :: (PerformsIO r, SendsPackets r, HasWorld r, Logs r) => Eff (Player ': r) a -> Player (Eff r a)

-- Logging to console without overwriting all the other threads that are also logging to console is an effect
type Logs r = Member Logging r

data Logging a where
  -- Log a string at a given Log Level
  LogString :: LogLevel -> String -> Logging ()
  -- Commute a Logging effect out of a stack
  ForkLogger :: (Configured r,PerformsIO r) => Eff (Logging ': r) a -> Logging (Eff r a)

-- TODO: replace this with a `data LogSpec = LogSpec {spec :: String -> String,level :: Int}`?
-- Level of verbosity to log at
data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | VerboseLog | TaggedLog String | NormalLog deriving Eq

-- All the information about a mineman world. Notably, players is a Map of PId's to TVars of player data, not actual player data
data WorldData = WorldData {chunks :: Map ChunkCoord ChunkSection, entities :: Map EntityId (Some Entity), players :: Map PlayerId (TVar PlayerData), nextEID :: EntityId, nextUUID :: UUID}

-- Having access to the World is an effect
type HasWorld r = Member World r

data World a where
  -- Commute a World effect out of a stack
  ForkWorld :: (Logs r,PerformsIO r) => Eff (World ': r) a -> World (Eff r a)
  -- TODO: redesign effect to slim down the effect interface to more general combinators, and bulk up the
  -- outside API interface to do what this is doing right now
  GetChunk :: ChunkCoord -> World ChunkSection
  RemoveBlock :: BlockCoord -> World ()
  SetBlock :: Some Block -> BlockCoord -> World ()
  SetChunk :: ChunkSection -> ChunkCoord -> World ()
  SetColumn :: [ChunkSection] -> (Int,Int) -> Maybe BS.ByteString -> World ()
  WorldSTM :: STM a -> World a
  FreshEID :: World EntityId
  FreshUUID :: World UUID
  SetPlayer :: PlayerId -> PlayerData -> World ()
  GetPlayer :: PlayerId -> World PlayerData
  NewPlayer :: TVar PlayerData -> World PlayerId
  GetEntity :: EntityId -> World (Some Entity)
  DeleteEntity :: EntityId -> World ()
  SummonMob :: (Some Mob) -> World ()
  SummonObject :: (Some Object) -> World ()
  AllPlayers :: World [PlayerData]
  BroadcastPacket :: ForAny OutboundPacket -> World ()

-- Things that can be serialized into a BS for the network
class Serialize s where
  serialize :: s -> BS.ByteString

-- Combinator for serializing Maybes in a Minecrafty way
serOpt :: Serialize s => Maybe s -> BS.ByteString
serOpt (Just a) = serialize True <> serialize a
serOpt Nothing = serialize False

-- Instances for haskell and library types
instance Serialize NBT where
  serialize = Ser.encode

instance Serialize Word8 where
  serialize = BS.singleton

instance Serialize [Char] where
  serialize str = serialize ((fromIntegral $ BS.length encoded) :: VarInt) `BS.append` encoded
    where
      encoded = Data.ByteString.UTF8.fromString str

instance Serialize Bool where
  serialize True = BS.singleton 0x01
  serialize False = BS.singleton 0x00

instance Serialize Int16 where
  serialize i = BS.pack $ map ((unsafeCoerce :: Int16 -> Word8) . shiftR i) [8,0]

instance Serialize Int32 where
  serialize i = BS.pack $ map ((unsafeCoerce :: Int32 -> Word8) . shiftR i) [24,16..0]

instance Serialize Float where
  serialize = serialize . (unsafeCoerce :: Float -> Int32)

instance Serialize Int64 where
  serialize i = BS.pack $ map ((unsafeCoerce :: Int64 -> Word8) . shiftR i) [56,48..0]

instance Serialize Word64 where
  serialize = serialize . (unsafeCoerce :: Word64 -> Int64)

instance Serialize Double where
  serialize = serialize . (unsafeCoerce :: Double -> Int64)

-- Annotate a BS with its length as a VarInt
withLength :: BS.ByteString -> BS.ByteString
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) <> bs

-- Annotate a list of serializable things with the length of the list
withListLength :: Serialize s => [s] -> BS.ByteString
withListLength ls = serialize ((fromIntegral $ length ls) :: VarInt) <> BS.concat (map serialize ls)

-- NOTE: Ambiguous type on packetName, parsePacket, and packetId
-- With TypeApplications we can do for example `packetId @Server.UseEntity`
-- This applies `Server.UseEntity` to the capital lambda "forall":
--
-- packetName :: forall p. Packet p => String
--
-- and we substitute `Server.UseEntity` for `p`, and get `:: Packet Server.UseEntity => String`
-- which brings the correct representation "into scope" via dictionary

-- Class of types that are packets
-- TODO: Split into Client/Server subclasses so we don't have lots of `undefined` parsePacket and onPacket everywhere
class Packet p where
  -- State this packet is expected in
  type PacketState p :: ServerState
  -- Extract name-value pairs to pretty print a packet
  packetPretty :: p -> [(String,String)]
  -- Name of the packet. Example `data Meow = ...` has the name "Meow"
  packetName :: String
  -- Packet Id -- Put this in the type level? Fundep: `packetstate, packetId -> p`
  packetId :: VarInt

-- Helper classes for using Data.SuchThat with TypeFamilies, which would otherwise be partially applied
class Packet p => HandledPacket p where
  -- Spawn a PLT on receipt of each packet
  -- TODO: remove arbitrary IO in favor of STM or something else
  onPacket :: (Configured r,SendsPackets r,PerformsIO r,Logs r,HasPlayer r,HasWorld r) => p -> Eff r ()
  -- No default defn. here so that we get compiler warnings for not implmenting
  --onPacket p = send (LogString ErrorLog $ "Unsupported packet: " ++ showPacket p)
  -- Parse a packet
  --parsePacket :: Parser p
  -- Deprecated for Serial
  -- Can `onPacket` possibly poke the packet state, so we need to wait for it to
  -- finish in serial, not parallel?
  canPokePacketState :: Bool
  canPokePacketState = False

class (HandledPacket p,PacketState p ~ s) => HP s p | p -> s where {}
instance (HandledPacket p,PacketState p ~ s) => HP s p where {}

class (Packet p,s ~ PacketState p) => PacketWithState s p | p -> s where {}
instance (Packet p,s ~ PacketState p) => PacketWithState s p where {}

-- We can't make this an actual `Show` instance without icky extentions. Also,
-- the instance context is not checked during instance selection, so we would
-- have to make a full on `instance Show a where`, which is obviously bad.
showPacket :: forall p. Packet p => p -> String
showPacket pkt = formatPacket (packetName @p) (packetPretty pkt)

-- A `InboundPacket s` is a thing that is a HandledPacket with the given PacketState
type InboundPacket s = SuchThatStar '[PacketWithState s,HandledPacket]

-- A `OutboundPacket s` is a thing that is a ClientPacket with the given PacketState
type OutboundPacket s = SuchThatStar '[PacketWithState s,Serialize]

-- Entities are things with properties
class Entity m where
  entityName :: String
  entityType :: VarInt
  entitySize :: m -> (Float,Float,Float)
  entityLocation :: m -> EntityLocation
  entityVelocity :: m -> EntityVelocity
  entityMeta :: m -> [EntityMetadata]

-- Some entities are Mobs
class Entity m => Mob m where {}

-- Some entities are Objects, which can haev special, different properties
class Entity m => Object m where
  objectName :: String
  objectId :: Word8
  -- TODO: does objectSize always == entitySize? If so, remove objectSize
  objectSize :: m -> (Float,Float,Float)
  objectSize = entitySize
  -- TODO: does objectLocation always == entityLocation? If so, remove objectLocation
  objectLocation :: m -> EntityLocation
  objectLocation = entityLocation
  objectData :: m -> (Int32,Maybe EntityVelocity)

-- EntityMeta is just a normal value with special type properties
newtype EntityMeta p = EntityMeta p

-- Those special properties are that the type has an instance for `EntityMetaType`
type EntityMetadata = SuchThat '[EntityMetaType] EntityMeta

-- Helper data structures to reduce reptition
data EntityLocation = EntityLocation (Double,Double,Double) (Word8,Word8) 
data EntityVelocity = EntityVelocity (Short,Short,Short)

-- EntityMeta can be Serialized in a special way, involving the `entityMetaFlag`
class Serialize (EntityMeta p) => EntityMetaType p where entityMetaFlag :: Word8

-- Serializing a bunch of EntityMetadatas involves indexing, so Nothing means that is absent, but it still increments the index counter
instance Serialize [Maybe EntityMetadata] where
  serialize = go 0
    where
      go :: Word8 -> [Maybe EntityMetadata] -> BS.ByteString
      go ix (Just (SuchThat (EntityMeta (m::p))):xs) = serialize ix <> serialize (entityMetaFlag @p) <> serialize (EntityMeta m) <> go (succ ix) xs
      go ix (Nothing : xs) = go (succ ix) xs
      go _ [] = serialize (0xff :: Word8)

-- NYI: (4,Chat) (7,Rotation 3xFloat) (8,Position)

instance EntityMetaType Word8 where entityMetaFlag = 0x00
instance Serialize (EntityMeta Word8) where serialize (EntityMeta a) = serialize a

instance EntityMetaType VarInt where entityMetaFlag = 0x01
instance Serialize (EntityMeta VarInt) where serialize (EntityMeta a) = serialize a

instance EntityMetaType Float where entityMetaFlag = 0x02
instance Serialize (EntityMeta Float) where serialize (EntityMeta a) = serialize a

instance EntityMetaType String where entityMetaFlag = 0x03
instance Serialize (EntityMeta String) where serialize (EntityMeta a) = serialize a

instance EntityMetaType (Maybe Slot) where entityMetaFlag = 0x05
instance Serialize (EntityMeta (Maybe Slot)) where serialize (EntityMeta a) = serialize a

instance EntityMetaType Bool where entityMetaFlag = 0x06
instance Serialize (EntityMeta Bool) where serialize (EntityMeta a) = serialize a

instance EntityMetaType (Maybe BlockCoord) where entityMetaFlag = 0x09
instance Serialize (EntityMeta (Maybe BlockCoord)) where serialize (EntityMeta m) = serOpt m

instance EntityMetaType BlockFace where entityMetaFlag = 0x0A
instance Serialize (EntityMeta BlockFace) where serialize (EntityMeta f) = serialize @VarInt $ fromIntegral (fromEnum f)

instance EntityMetaType (Maybe UUID) where entityMetaFlag = 0x0B
instance Serialize (EntityMeta (Maybe UUID)) where serialize (EntityMeta m) = serOpt m

instance EntityMetaType (Maybe BlockState) where entityMetaFlag = 0x0C
instance Serialize (EntityMeta (Maybe BlockState)) where serialize (EntityMeta m) = serialize @VarInt $ case m of {Nothing -> 0;Just (BlockState bid dmg) -> unsafeCoerce $ shiftL bid 4 .|. unsafeCoerce dmg}

-- Having access to low-level networking operations is an effect
type Networks r = Member Networking r

data Networking a where
  -- Commute a Networking effect off a stack
  ForkNetwork :: PerformsIO r => Eff (Networking ': r) a -> Networking (Eff r a)
  -- Turn compression on and set the threshold
  SetCompressionLevel :: VarInt -> Networking ()
  -- Add compression (if enabled) and packet metadata. Should be used after adding packetId, but before sending
  AddCompression :: BS.ByteString -> Networking BS.ByteString
  -- Remove compression (if enabled) and strip packet metadata. Packets are left with packetId intact
  RemoveCompression :: BS.ByteString -> Networking BS.ByteString
  -- Turn encryption on using the given Shared Secret
  SetupEncryption :: BS.ByteString -> Networking ()
  -- Send bytes over the network, encrypting if enabled
  PutIntoNetwork :: BS.ByteString -> Networking ()
  -- Read bytes from the network, decrypting if enabled
  GetFromNetwork :: Int -> Networking BS.ByteString

-- Having access to a high-level networking interface is an effect
type SendsPackets r = Member Packeting r

data Packeting a where
  -- Send a packet for any packet state
  SendPacket :: ForAny OutboundPacket -> Packeting ()
  -- Send raw bytes over the network (used in LegacyHandshakePong)
  UnsafeSendBytes :: BS.ByteString -> Packeting ()
  BeginEncrypting :: BS.ByteString -> Packeting ()
  BeginCompression :: VarInt -> Packeting ()

-- Indent the hexdump; helper function
indentedHex :: BS.ByteString -> String
indentedHex = init . unlines . map ("  "++) . lines . prettyHex

-- Reading the configuration is an effect
type Configured r = Member (Reader Configuration) r

data Configuration = Configuration
  {shouldLog :: LogLevel -> Bool
  ,spawnLocation :: BlockCoord
  ,defaultDifficulty :: Difficulty
  ,defaultDimension :: Dimension
  ,defaultGamemode :: Gamemode
  ,maxPlayers :: Word8
  ,compressionThreshold :: Maybe VarInt
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  {shouldLog = \case {HexDump -> False; _ -> True}
  ,spawnLocation = BlockLocation (0,64,0)
  ,defaultDifficulty = Peaceful
  ,defaultDimension = Overworld
  ,defaultGamemode = Survival
  -- TODO: Check against this
  ,maxPlayers = 100
  ,compressionThreshold = Just 16
  }

forkConfig :: Configured q => Eff (Reader Configuration ': r) a -> Eff q (Eff r a)
forkConfig e = do
  c <- ask
  return $ runReader' c e

comb :: Some c -> (forall a. c a => r) -> r
comb (SuchThat (Identity (_ :: a))) f = f @a
