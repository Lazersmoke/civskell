{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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

module Civskell.Data.Types
  (module Civskell.Data.Common
  ,module Civskell.Data.Protocol
  ,module Civskell.Data.Types
  ,module Civskell.Data.Entity
  ,module Civskell.Data.Block
  ,module Civskell.Data.World
  ,module Civskell.Data.Item
  ,module Civskell.Data.Player
  ,module Civskell.Data.Chunk
  ) where

import Civskell.Data.Common
import Civskell.Data.Protocol
import Civskell.Data.Entity
import Civskell.Data.Block
import Civskell.Data.World
import Civskell.Data.Item
import Civskell.Data.Player
import Civskell.Data.Chunk

import qualified Data.Map.Lazy as Map
import System.IO
import qualified Network as Net
import Data.Maybe (fromMaybe)
import Data.Map.Lazy (Map)
import Data.Semigroup
import Data.Void
import Control.Lens
import Control.Monad.Reader
import Control.Concurrent.STM
--import Control.Monad
import Data.Bits
import Data.Text (Text)
import qualified Data.Text as T
import Data.SuchThat
import Data.Word
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import qualified Data.Vector as Vector
import qualified Data.Array.IArray as Arr
import qualified Data.Set as Set

-- | The base monad for Civskell
type Civskell a = ReaderT CivskellContext IO a

data CivskellContext = CivskellContext
  {configuration :: Configuration
  ,worldData :: TVar WorldData
  ,globalLogQueue :: LogQueue
  ,playerData :: TVar PlayerData
  ,networkCompressionThreshold :: TVar (Maybe VarInt)
  ,networkEncryptionCouplet :: TVar (Maybe EncryptionCouplet)
  ,networkHandle :: Handle
  }

fromContext :: (CivskellContext -> TVar a) -> Civskell a
fromContext n = asks n >>= lift . readTVarIO

overContext :: (CivskellContext -> TVar a) -> (a -> a) -> Civskell ()
overContext n f = asks n >>= lift . atomically . flip modifyTVar f

-- This is a massive hack in need of a better solution.
-- We probably need to parameterize CivskellContext and Civskell over their scope.
fakeGlobalContext :: Configuration -> TVar WorldData -> LogQueue -> CivskellContext
fakeGlobalContext c wor lq = CivskellContext
  {configuration = c
  ,worldData = wor
  ,globalLogQueue = lq
  ,playerData = undefined
  ,networkCompressionThreshold = undefined
  ,networkEncryptionCouplet = undefined
  ,networkHandle = undefined
  }

initWorld :: WorldData
initWorld = WorldData
  {_worldChunks = Map.empty
  ,_worldGenerator = emptyWorldGenerator
  ,_worldEntities = Map.empty
  ,_worldPlayers = Map.empty
  ,_worldNextEID = 0
  ,_worldNextUUID = UUID (0,1)}

summonObject :: Satisfies Entity -> Civskell ()
summonObject e' = asks worldData >>= \wor -> lift . atomically . modifyTVar wor $ \w -> (worldEntities . at (view worldNextEID w) .~ Just e') . (worldNextEID %~ succ) . (worldNextUUID %~ succ) $ w

getInventorySlot :: Short -> Civskell (ForAny Slot)
getInventorySlot i = (Vector.! fromIntegral i) . view playerInventory <$> fromContext playerData

setInventorySlot :: Short -> Slot i -> Civskell ()
setInventorySlot i s = overContext playerData $ playerInventory %~ (Vector.// [(fromIntegral i,ambiguate s)])

-- | Gets the slot in a player's hand, accounting for selected hotbar slot
slotInHand :: Hand -> Civskell Short
slotInHand = \case
  MainHand -> (36+) . view playerHoldingSlot <$> fromContext playerData
  OffHand -> pure 45

-- TODO: replace this with a `data LogSpec = LogSpec {spec :: String -> String,level :: Int}`?
-- Level of verbosity to log at
data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | NYILog | VerboseLog | TaggedLog Text | NormalLog deriving (Show,Eq)

-- | Returns the standard logging prefix for the specitifed log level and configuration
getLogBadge :: Configuration -> LogLevel -> Text
getLogBadge c = \case
  HexDump -> "[Hexdump] "
  ClientboundPacket -> "[\x1b[32mSent\x1b[0m] "
  ServerboundPacket -> "[\x1b[32mRecv\x1b[0m] "
  ErrorLog -> "[\x1b[31m\x1b[1mError\x1b[0m] "
  NYILog -> "[\x1b[34mNYI\x1b[0m] "
  VerboseLog -> "[\x1b[36m" <> serverName c <> "/Verbose\x1b[0m] "
  (TaggedLog tag) -> "[\x1b[36m" <> tag <> "\x1b[0m] "
  NormalLog -> "[\x1b[36m" <> serverName c <> "\x1b[0m] "

-- | A @'PacketHandler' p@ is a description of how to deserialize and handle packets of type @p@.
data PacketHandler p = PacketHandler 
  {packetThreadingMode :: ThreadingMode
  ,onPacket :: p -> Civskell ()
  ,deserializePacket :: forall m. MonadGet m => m p
  }

-- | An @'InboundPacketDescriptor'@ is a 
type InboundPacketDescriptor = PacketDescriptor PacketHandler

data Configuration = Configuration
  {shouldLog :: LogLevel -> Bool
  ,protocolVersion :: Integer
  ,serverVersion :: Text
  ,serverName :: Text
  ,serverPort :: Net.PortID
  ,serverMotd :: Text
  ,spawnLocation :: BlockCoord
  ,defaultDifficulty :: Difficulty
  ,defaultDimension :: Dimension
  ,defaultGamemode :: Gamemode
  ,maxPlayers :: Word8
  ,compressionThreshold :: Maybe VarInt
  ,packetsForState :: ServerState -> SupportedPackets PacketHandler
  ,supportedItems :: SupportedItems
  ,supportedBlocks :: SupportedBlocks
  ,keepAliveThread :: Civskell Void
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  {shouldLog = \case {HexDump -> False; VerboseLog -> False; _ -> True}
  ,protocolVersion = 0
  ,serverVersion = "Default"
  ,serverName = "Civskell"
  ,serverPort = Net.PortNumber 25565
  ,serverMotd = "An Experimental Minecraft server written in Haskell | github.com/Lazersmoke/civskell"
  ,spawnLocation = BlockLocation (0,64,0)
  ,defaultDifficulty = Peaceful
  ,defaultDimension = Overworld
  ,defaultGamemode = Survival
  -- TODO: Check against this
  ,maxPlayers = 100
  ,compressionThreshold = Just 16
  ,packetsForState = const Vector.empty
  ,supportedItems = Map.empty
  ,supportedBlocks = Map.empty
  ,keepAliveThread = keepAliveThread defaultConfiguration
  }

displayConfig :: Configuration -> Text
displayConfig c@Configuration{..} = T.intercalate "\n"
  [{-Server name shown in tag-}"version " <> serverVersion <> " (Protocol " <> showText protocolVersion <> ")"
  ,"Motd: " <> serverMotd
  ,"Port: " <> showText serverPort
  ,"Enabled logging badges: " <> T.intercalate ", " (filter (not . T.null) $ map (getLogBadge c) logLevels)
  ,"Default: " <> showText spawnLocation <> " in the " <> showText defaultDimension <> " | " <> showText defaultGamemode <> " | " <> showText defaultDifficulty
  ,"Max players: " <> showText maxPlayers
  ,"Handshake packets:\n" <> T.intercalate ", " (map (ambiguously packetName) . Vector.toList $ packetsForState Handshaking)
  ,"LoggingIn packets:\n" <> T.intercalate ", " (map (ambiguously packetName) . Vector.toList $ packetsForState LoggingIn)
  ,"Play packets:\n" <> T.intercalate ", " (map (ambiguously packetName) . Vector.toList $ packetsForState Playing)
  ,"Status packets:\n" <> T.intercalate ", " (map (ambiguously packetName) . Vector.toList $ packetsForState Status)
  ]
  where
    logLevels = [HexDump,ClientboundPacket,ServerboundPacket,ErrorLog,NYILog,VerboseLog,TaggedLog "Tag",NormalLog]

-- A thing that shuffles log messages off of running threads and logs that whenever on a dedicated one.
newtype LogQueue = LogQueue (TQueue Text)

-- A chunk full of air (used in chunk serialization to fill in gaps in the Map)
--airChunk :: Map BlockOffset (ForAny Block)
--airChunk = Map.fromList [(BlockLocation (x,y,z),Air) | x <- [0..15], z <- [0..15], y <- [0..15]]

-- | A mapping from Block Ids to decodings for those blocks.
type SupportedBlocks = Map BlockId (ForAny BlockBuilder)

-- | A single implemented decoder for a single type of block.
newtype BlockBuilder b = BlockBuilder {buildBlock :: Short -> Nibble -> Block b}

-- | Given a set of @'SupportedBlocks'@ and a raw @'WireBlock'@ from the network,
-- parse out a @'Block'@ of some type.
parseBlockFromSet :: SupportedBlocks -> WireBlock -> Maybe (ForAny Block)
parseBlockFromSet sb (WireBlock bId meta) = case Map.lookup bId sb of
  Just (SuchThat b) -> Just . ambiguate $ buildBlock b bId meta
  Nothing -> Nothing

-- | Just like @'parseBlockFromSet'@, but it gets the @'SupportedBlocks'@ from the @'Configuration'@.
-- See @'supportedBlocks'@.
parseBlockFromContext :: WireBlock -> Civskell (Maybe (ForAny Block))
parseBlockFromContext wb = asks configuration >>= \c -> pure $ parseBlockFromSet (supportedBlocks c) wb

-- TODO: Transactional

-- | Clear any pending teleport with the given TPId.
-- Returns @'True'@ if the given TPId was cleared, 
-- or @'False'@ if it wasn't there in the first place.
clearTeleport :: VarInt -> Civskell Bool
clearTeleport tid = do
  pla <- playerData <$> ask
  lift . atomically $ do
    present <- Set.member tid . view playerTPConfirmQueue <$> readTVar pla
    modifyTVar pla $ playerTPConfirmQueue %~ Set.delete tid
    pure present

-- | Broadcast a packet to all currently @'Playing'@ players.
broadcastPacket :: DescribedPacket PacketSerializer p -> Civskell ()
broadcastPacket pkt = do
  worldTvar <- worldData <$> ask
  -- For each player in the world, send the packet to that player
  lift . atomically $ mapM_ sendPktForPlayer . view worldPlayers =<< readTVar worldTvar
  where
    sendPktForPlayer pdt = readTVar pdt >>= \pd -> case view playerState pd of
      -- Only send it to Playing players
      Playing -> writeTQueue (view playerPacketQueue pd) (ambiguate pkt)
      _ -> pure ()

-- | Just like @'parseItemFromSet'@, but get the @'SupportedItems'@ from the @'Configuration'@'s @'supportedItems'@ field.
parseItemFromContext :: WireSlot -> Civskell (Maybe (ForAny Slot))
parseItemFromContext ws = asks configuration >>= \c -> pure $ parseItemFromSet (supportedItems c) ws
-- | Given a function to convert an item into the block to be placed, this is the standard callback for placing an item as a block.
placeBlock :: BlockDescriptor i -> ItemUseCallback i
placeBlock desc = Just (\(SlotData i ic) -> Slot . Just $ SlotData i (ic - 1),setTheBlock)
  where
    setTheBlock (Item _iDesc iDat) bc bf _ _ = do
      wdTvar <- worldData <$> ask
      lift $ atomically $ modifyTVar wdTvar (blockInWorld (blockOnSide bc bf) .~ ambiguate (Block desc iDat))

-- Abstraction methods for Inventory, not really needed tbh.
--getSlot :: Short -> Inventory -> ForAny Slot
--getSlot = Map.findWithDefault (ambiguate $ Slot Nothing)

--inventorySlot :: Short -> Lens' Inventory (ForAny Slot)
--inventorySlot s = at s . to (fromMaybe (ambiguate $ Slot Nothing))

-- | These types are isomorphic because @'SuchThat' ('Slot' 'Nothing')@ is a default for @'ForAny' 'Slot'@.
slotMaybe :: Iso' (Maybe (ForAny Slot)) (ForAny Slot)
slotMaybe = iso mSlotToSlot slotToMSlot
  where
    mSlotToSlot :: Maybe (ForAny Slot) -> ForAny Slot
    mSlotToSlot Nothing = ambiguate $ Slot Nothing
    mSlotToSlot (Just s) = s

    slotToMSlot :: ForAny Slot -> Maybe (ForAny Slot)
    slotToMSlot (SuchThat (Slot (Just sd))) = Just (SuchThat (Slot (Just sd)))
    slotToMSlot (SuchThat (Slot Nothing)) = Nothing


--setSlot :: Short -> Slot i -> Inventory -> Inventory
--setSlot k (Slot Nothing) = Map.delete k
--setSlot k s = Map.insert k (ambiguate s)

-- Sidestep existential nonsense when creating items
--slot :: Item i -> Word8 -> Slot i
--slot i c = Slot . Just $ SlotData i c

--slotAmount :: Slot i -> Word8
--slotAmount (Slot (Just (SlotData _ c))) = c
--slotAmount (Slot Nothing) = 0

-- Remove as many items as possible, up to count, return amount removed
--removeCount :: Word8 -> Slot i -> (Word8,Slot i)
--removeCount _ (Slot Nothing) = (0,Slot Nothing)
--removeCount c (Slot (Just (SlotData i count))) = if c < count then (c,Slot . Just $ SlotData i (count - c)) else (count,Slot Nothing)

-- Count is how many to put into first stack from second
--splitStack :: Word8 -> Slot i -> (Slot i,Slot i)
--splitStack _ (Slot Nothing) = (Slot Nothing,Slot Nothing)
--splitStack c s@(Slot (Just (SlotData i _))) = (firstStack,secondStack)
  --where
    --firstStack = if amtFirstStack == 0 then Slot Nothing else Slot . Just $ SlotData i amtFirstStack
    --(amtFirstStack,secondStack) = removeCount c s

-- | Take the specified number of items out of a @'SlotData'@, and put them into a new @'SlotData'@, leaving a (possibly empty) @'Slot'@
takeFromSlot :: Word8 -> SlotData i -> (SlotData i,Slot i)
takeFromSlot c (SlotData i ic) = if c < ic
  then (SlotData i c,Slot (Just (SlotData i (ic - c))))
  else (SlotData i ic,Slot Nothing)

-- | Legacy Handshake packet constant. This is what we send when we get a legacy ping from a client.
legacyHandshakePongConstant :: BS.ByteString
legacyHandshakePongConstant = BS.pack [0xFF,0x00,0x1b,0x00,0xa7
  ,0x00,0x31 -- 1
  ,0x00,0x00 -- Seperator
  ,0x00,0x33 -- 3 -- Protocol Version number. TODO: Update this with protocolVersion
  ,0x00,0x33 -- 3
  ,0x00,0x35 -- 5
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
  ,0x00,0x7C -- <pipe>
  ,0x00,0x50 -- P
  ,0x00,0x69 -- i
  ,0x00,0x6E -- n
  ,0x00,0x67 -- g
  ,0x00,0x48 -- H
  ,0x00,0x6F -- o
  ,0x00,0x73 -- s
  ,0x00,0x74 -- t
  ]

-------------------------
-- Packet Helper Types --
-------------------------
-- Should these be moved next to their respective packet definitions in
-- Civskell.Packet.*bound?

-- Used in Client.Animation; Server.Animation just uses hand
data AnimationAction = SwingHand Hand | Critical Bool | TakeDamage | LeaveBedAnimation deriving Show

instance Serial AnimationAction where
  serialize x = putWord8 $ case x of
    (SwingHand MainHand) -> 0x00
    TakeDamage -> 0x01
    LeaveBedAnimation -> 0x02
    (SwingHand OffHand) -> 0x03
    (Critical False) -> 0x04
    (Critical True) -> 0x05
  deserialize = getWord8 >>= pure . \case
    0x00 -> SwingHand MainHand
    0x01 -> TakeDamage
    0x02 -> LeaveBedAnimation
    0x03 -> SwingHand OffHand
    0x04 -> Critical False
    0x05 -> Critical True
    _ -> error "Bad Deserialize on AnimationAction"

-- Used in Server.UseEntity
data EntityInteraction = Attack | Interact Hand | InteractAt (Float,Float,Float) Hand deriving Show

instance Serial EntityInteraction where
  serialize = \case
    Interact hand -> serialize @VarInt 0 *> serialize @Hand hand
    Attack -> serialize @VarInt 1
    InteractAt loc hand -> serialize @VarInt 2 *> serialize loc *> serialize @Hand hand
  deserialize = deserialize @VarInt >>= \case
    0 -> Interact <$> deserialize @Hand
    1 -> pure Attack
    2 -> InteractAt <$> deserialize @(Float,Float,Float) <*> deserialize @Hand
    x -> error $ "deserialize @EntityInteraction: Got (VarInt) " <> show x

-- Used in Server.PlayerDigging
data PlayerDigAction = StartDig BlockCoord BlockFace | StopDig BlockCoord BlockFace | EndDig BlockCoord BlockFace | DropItem Bool | ShootArrowOrFinishEating | SwapHands

instance Serial PlayerDigAction where
  serialize = \case
    StartDig bc bf -> serialize @VarInt 0 *> serialize bc *> serialize bf
    StopDig bc bf -> serialize @VarInt 1 *> serialize bc *> serialize bf
    EndDig bc bf -> serialize @VarInt 2 *> serialize bc *> serialize bf
    DropItem True -> serialize @VarInt 3
    DropItem False -> serialize @VarInt 4
    ShootArrowOrFinishEating -> serialize @VarInt 5
    SwapHands -> serialize @VarInt 6
  deserialize = deserialize @VarInt >>= \case
    0 -> StartDig <$> deserialize @BlockCoord <*> deserialize @BlockFace
    1 -> StopDig <$> deserialize @BlockCoord <*> deserialize @BlockFace
    2 -> EndDig <$> deserialize @BlockCoord <*> deserialize @BlockFace
    3 -> pure (DropItem True)
    4 -> pure (DropItem False)
    5 -> pure ShootArrowOrFinishEating
    6 -> pure SwapHands
    x -> error $ "deserialize @PlayerDigAction: Got (VarInt) " <> show x

instance Show PlayerDigAction where
  show = \case
    StartDig bc bf -> "Start digging the " <> show bf <> " side of " <> show bc
    StopDig bc bf -> "Stop digging the " <> show bf <> " side of " <> show bc
    EndDig bc bf -> "End digging the " <> show bf <> " side of " <> show bc
    DropItem True -> "Dropped item stack"
    DropItem False -> "Dropped item (not stack)"
    ShootArrowOrFinishEating -> "Shot arrow *or* finished eating :thinking:"
    SwapHands -> "Swapped hands"

-- Used in Server.EntityAction
data PlayerEntityAction = Sneak Bool | Sprint Bool | HorseJumpStart VarInt | HorseJumpStop | LeaveBed | ElytraFly | HorseInventory

instance Serial PlayerEntityAction where
  serialize = \case
    Sneak True -> serialize @VarInt 0
    Sneak False -> serialize @VarInt 1
    LeaveBed -> serialize @VarInt 2
    Sprint True -> serialize @VarInt 3
    Sprint False -> serialize @VarInt 4
    HorseJumpStart jmp -> serialize @VarInt 5 >> serialize @VarInt jmp
    HorseJumpStop -> serialize @VarInt 6
    HorseInventory -> serialize @VarInt 7
    ElytraFly -> serialize @VarInt 8
  deserialize = deserialize @VarInt >>= \case
    0 -> pure (Sneak True)
    1 -> pure (Sneak False)
    2 -> pure LeaveBed
    3 -> pure (Sprint True)
    4 -> pure (Sprint False)
    5 -> HorseJumpStart <$> deserialize @VarInt
    6 -> pure HorseJumpStop
    7 -> pure HorseInventory
    8 -> pure ElytraFly
    x -> error $ "deserialize @PlayerEntityAction: Got (VarInt) " <> show x

instance Show PlayerEntityAction where
  show = \case
    Sneak True -> "Started sneaking"
    Sneak False -> "Stopped sneaking"
    LeaveBed -> "Left the bed"
    Sprint True -> "Start sprinting"
    Sprint False -> "Stop sprinting"
    HorseJumpStart jmp -> "Horse jump started with boost " <> show jmp
    HorseJumpStop -> "Horse jump ended"
    HorseInventory -> "Horse inventory opened"
    ElytraFly -> "Started flying with Elytra"

-- Used in Server.ClickWindow
data InventoryClickMode = NormalClick Bool | ShiftClick Bool | NumberKey Word8 | MiddleClick | ItemDropOut Bool | PaintingMode Word8 | DoubleClick deriving Show

-- Used in Server. and Client.PlayerAbilities
-- Invuln, Flying, Allow Flying, Creative
data AbilityFlags = AbilityFlags Bool Bool Bool Bool

instance Serial AbilityFlags where
  serialize (AbilityFlags i f af c) = serialize $ u i 0 .|. u f 1 .|. u af 2 .|. u c 3
    where
      u b = shiftL (unsafeCoerce b :: Word8)
  deserialize = (\w -> AbilityFlags (testBit w 0) (testBit w 1) (testBit w 2) (testBit w 3)) <$> getWord8

-- Used in Client.ChangeGameState
data GameStateChange = InvalidBed | Raining Bool | ChangeGamemode Gamemode | ExitTheEnd Bool | DemoMessage | ArrowHitOtherPlayer | FadeValue Float | FadeTime Float | ElderGuardian

instance Serial GameStateChange where
  serialize InvalidBed = putWord8 0x00 *> serialize (0 :: Float)
  serialize (Raining isStarting) = putWord8 (if isStarting then 0x01 else 0x02) *> serialize (0 :: Float)
  serialize (ChangeGamemode g) = putWord8 0x03 *> serialize (case g of {Survival -> 0; Creative -> 1;} :: Float)
  serialize (ExitTheEnd showCredits) = putWord8 0x04 *> if showCredits then serialize (1 :: Float) else serialize (0 :: Float)
  serialize DemoMessage = putWord8 0x05 *> serialize (0 :: Float)
  serialize ArrowHitOtherPlayer = putWord8 0x06 *> serialize (0 :: Float)
  serialize (FadeValue f) = putWord8 0x07 *> serialize f
  serialize (FadeTime f) = putWord8 0x08 *> serialize f
  serialize ElderGuardian = putWord8 0x09 *> serialize (0 :: Float)
  deserialize = getWord8 >>= \case
    0x00 -> deserialize @Float >> pure InvalidBed
    0x01 -> deserialize @Float >> pure (Raining True)
    0x02 -> deserialize @Float >> pure (Raining False)
    0x03 -> deserialize @Float >>= pure . ChangeGamemode . \case
      0 -> Survival
      1 -> Creative
      x -> error $ "Invalid Gamemode in ChangeGameState: " <> show x
    0x04 -> deserialize @Float >>= pure . ExitTheEnd . \case
      0 -> False
      1 -> True
      x -> error $ "Invalid showCredits in GameStateChange: " <> show x
    0x05 -> deserialize @Float >> pure DemoMessage
    0x06 -> deserialize @Float >> pure ArrowHitOtherPlayer
    0x07 -> deserialize @Float >>= pure . FadeValue
    0x08 -> deserialize @Float >>= pure . FadeTime
    0x09 -> deserialize @Float >> pure ElderGuardian
    x -> error $ "Invalid ChangeGameState with flag: " <> show x

-- Used in Server.ClientAction
data ClientStatusAction = PerformRespawn | RequestStats | OpenInventory deriving (Show,Enum)

instance Serial ClientStatusAction where
  serialize = serialize @VarInt . fromIntegral . fromEnum
  deserialize = toEnum . fromIntegral <$> (deserialize @VarInt)

blockInWorld :: BlockCoord -> Lens' WorldData (ForAny Block)
blockInWorld bc = worldChunks . at (blockToChunk bc) . iso (fromMaybe emptyChunk) Just . blockInChunk (blockToRelative bc)

blockInChunk :: BlockOffset -> Lens' (ChunkSection b) b
blockInChunk bo = _Wrapped . lens (Arr.! targetCoord) (\a e' -> a Arr.// [(targetCoord,e')])
  where
    targetCoord = (fromIntegral x,fromIntegral y,fromIntegral z) :: (Nibble,Nibble,Nibble)
    (BlockLocation (x,y,z)) = bo

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
-- Get the block coord adjacent to a given coord on a given side
blockOnSide :: BlockCoord -> BlockFace -> BlockCoord
blockOnSide (BlockLocation (x,y,z)) Bottom = BlockLocation (x,y-1,z)
blockOnSide (BlockLocation (x,y,z)) Top = BlockLocation (x,y+1,z)
blockOnSide (BlockLocation (x,y,z)) (SideFace North) = BlockLocation (x,y,z-1)
blockOnSide (BlockLocation (x,y,z)) (SideFace South) = BlockLocation (x,y,z+1)
blockOnSide (BlockLocation (x,y,z)) (SideFace West) = BlockLocation (x-1,y,z)
blockOnSide (BlockLocation (x,y,z)) (SideFace East) = BlockLocation (x+1,y,z)


data Event = Event

-- | A callback for when a player right-clicks a block.
type BlockClickCallback b 
  =  b -- ^ The block they clicked
  -> BlockCoord -- ^ The @'BlockCoord'@ of the block
  -> BlockFace -- ^ The @'BlockFace'@ they clicked on
  -> Hand -- ^ The @'Hand'@ they clicked with
  -> (Float,Float,Float) -- ^ The part of the block they clicked
  -> Civskell ()

-- | A callback for when a player right-clicks with an @'Item'@. See @'onItemUse'@.
type ItemUseCallback i = Maybe (SlotData i -> Slot i,Item i -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Civskell ())

-- | A callback for when a player clicks a slot in a window.
type WindowClickCallback w =
  w -- ^ The window they clicked.
  -> WindowId -- ^ The @'WindowId'@ of the window they clicked.
  -> Short -- ^ The slot number in the window they clicked.
  -> TransactionId -- ^ The @'TransactionId'@ of this click.
  -> InventoryClickMode -- ^ The manner in which they clicked the slot.
  -> WireSlot -- ^ The client provided @'WireSlot'@ that they think is in the slot they clicked.
  -> Civskell Bool -- ^ Return @'True'@ if the transaction succeeded, @'False'@ otherwise. They will be required to apologize if the transaction is rejected.
