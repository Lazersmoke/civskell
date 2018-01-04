{-# LANGUAGE LambdaCase #-}
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

-- TODO: This export list is pretty scary. We should make tiered modules or something
module Civskell.Data.Types
  (module Civskell.Data.Common
  ,module Civskell.Data.Types
  ) where

import Civskell.Data.Common

import GHC.Generics hiding (to)
import Data.List (unfoldr)
import qualified Data.Binary.BitBuilder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as Map
import System.IO
import Data.Maybe (fromMaybe)
import Data.Map.Lazy (Map)
import Data.Semigroup
import Data.Foldable
import Control.Lens
import Control.Monad.Reader
import Control.Concurrent.STM
--import Control.Monad
import Crypto.Cipher.AES (AES128)
import Data.Aeson (withObject,(.:),(.:?),(.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types
import Data.Bits
import Data.Int
import Data.NBT
import Data.Text (Text)
import qualified Data.Text as T
import Data.SuchThat
import Data.String
import qualified GHC.Exts as Exts
import Data.Word
import Hexdump (prettyHex)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8
import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import qualified Data.Serialize as Cereal
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
initWorld = WorldData {_chunks = Map.empty,_entities = Map.empty,_players = Map.empty,_nextEID = 0,_nextUUID = UUID (0,1)}

-- All the information about a mineman world. Notably, players is a Map of PId's to TVars of player data, not actual player data
data WorldData = WorldData 
  {_chunks :: Map ChunkCoord (ChunkSection (ForAny Block))
  ,_entities :: Map EntityId (Satisfies Entity)
  ,_players :: Map PlayerId (TVar PlayerData)
  ,_nextEID :: EntityId
  ,_nextUUID :: UUID
  }

worldChunks :: Lens' WorldData (Map ChunkCoord (ChunkSection (ForAny Block)))
worldChunks f wd = (\x -> wd {_chunks = x}) <$> f (_chunks wd)

worldEntities :: Lens' WorldData (Map EntityId (Satisfies Entity))
worldEntities f wd = (\x -> wd {_entities = x}) <$> f (_entities wd)

worldPlayers :: Lens' WorldData (Map PlayerId (TVar PlayerData))
worldPlayers f wd = (\x -> wd {_players = x}) <$> f (_players wd)

worldNextEID :: Lens' WorldData EntityId
worldNextEID f wd = (\x -> wd {_nextEID = x}) <$> f (_nextEID wd)

worldNextUUID :: Lens' WorldData UUID
worldNextUUID f wd = (\x -> wd {_nextUUID = x}) <$> f (_nextUUID wd)

summonObject :: Satisfies Entity -> Civskell ()
summonObject e' = asks worldData >>= \wor -> lift . atomically . modifyTVar wor $ \w -> (worldEntities . at (view worldNextEID w) .~ Just e') . (worldNextEID %~ succ) . (worldNextUUID %~ succ) $ w

-- Stores all the relevant information about a player
-- TODO: Extensibility?
data PlayerData = PlayerData
  {_playerTPConfirmQueue :: Set.Set VarInt
  ,_playerNextTid :: VarInt
  ,_playerKeepAliveQue :: Set.Set VarInt
  ,_playerNextKid :: VarInt
  ,_playerNextWid :: WindowId
  -- Map of WindowId's to that window's metadata (including accessor effects)
  ,_playerWindows :: Map.Map WindowId (ForAny Window)
  ,_playerFailedTransactions :: Set.Set (WindowId,TransactionId)
  ,_playerHoldingSlot :: Short
  ,_playerPosition :: (Double,Double,Double)
  ,_playerViewAngle :: (Float,Float)
  ,_playerGamemode :: Gamemode
  -- player inventory is window 0
  ,_playerInventory :: Inventory
  ,_playerDiggingBlocks :: Map.Map BlockCoord BlockBreak
  ,_playerMoveMode :: MoveMode
  ,_playerState :: ServerState
  ,_playerUsername :: String
  ,_playerClientBrand :: String
  ,_playerClientUUID :: UUID
  ,_playerId :: PlayerId
  ,_playerPacketQueue :: TQueue (ForAny (DescribedPacket PacketSerializer))
  }

getInventorySlot :: Short -> Civskell (ForAny Slot)
getInventorySlot i = asks playerData >>= \pla -> (\(Just x) -> x) . view (playerInventory . at i) <$> lift (readTVarIO pla)

setInventorySlot :: Short -> Slot i -> Civskell ()
setInventorySlot i s = asks playerData >>= \pla -> lift . atomically . modifyTVar pla $ (playerInventory . at i) .~ Just (ambiguate s)

playerTPConfirmQueue :: Lens' PlayerData (Set.Set VarInt)
playerTPConfirmQueue = lens _playerTPConfirmQueue (\p x -> p {_playerTPConfirmQueue = x})

playerNextTid :: Lens' PlayerData VarInt
playerNextTid = lens _playerNextTid (\p x -> p {_playerNextTid = x})

playerWindows :: Lens' PlayerData (Map.Map WindowId (ForAny Window))
playerWindows = lens _playerWindows (\p x -> p {_playerWindows = x})

playerFailedTransactions :: Lens' PlayerData (Set.Set (WindowId,TransactionId))
playerFailedTransactions = lens _playerFailedTransactions (\p x -> p {_playerFailedTransactions = x})

playerHoldingSlot :: Lens' PlayerData Short
playerHoldingSlot = lens _playerHoldingSlot (\p x -> p {_playerHoldingSlot = x})

playerPosition :: Lens' PlayerData (Double,Double,Double)
playerPosition = lens _playerPosition (\p x -> p {_playerPosition = x})

playerViewAngle :: Lens' PlayerData (Float,Float)
playerViewAngle = lens _playerViewAngle (\p x -> p {_playerViewAngle = x})

playerGamemode :: Lens' PlayerData Gamemode
playerGamemode = lens _playerGamemode (\p x -> p {_playerGamemode = x})

playerInventory :: Lens' PlayerData Inventory
playerInventory = lens _playerInventory (\p x -> p {_playerInventory = x})

playerMoveMode :: Lens' PlayerData MoveMode
playerMoveMode = lens _playerMoveMode (\p x -> p {_playerMoveMode = x})

playerState :: Lens' PlayerData ServerState
playerState = lens _playerState (\p x -> p {_playerState = x})

playerUsername :: Lens' PlayerData String
playerUsername = lens _playerUsername (\p x -> p {_playerUsername = x})

playerClientBrand :: Lens' PlayerData String
playerClientBrand = lens _playerClientBrand (\p x -> p {_playerClientBrand = x})

playerClientUUID :: Lens' PlayerData UUID
playerClientUUID = lens _playerClientUUID (\p x -> p {_playerClientUUID = x})

playerPacketQueue :: Lens' PlayerData (TQueue (ForAny (DescribedPacket PacketSerializer)))
playerPacketQueue = lens _playerPacketQueue (\p x -> p {_playerPacketQueue = x})

-- TODO: replace this with a `data LogSpec = LogSpec {spec :: String -> String,level :: Int}`?
-- Level of verbosity to log at
data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | NYILog | VerboseLog | TaggedLog Text | NormalLog deriving Eq

data PacketHandler p = PacketHandler 
  {packetThreadingMode :: ThreadingMode
  ,onPacket :: p -> Civskell ()
  ,deserializePacket :: forall m. MonadGet m => m p
  }

type InboundPacketDescriptor = PacketDescriptor PacketHandler

data Configuration = Configuration
  {shouldLog :: LogLevel -> Bool
  ,protocolVersion :: Integer
  ,serverVersion :: Text
  ,serverName :: Text
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
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  {shouldLog = \case {HexDump -> False; _ -> True}
  ,protocolVersion = 0
  ,serverVersion = "Default"
  ,serverName = "Civskell"
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
  }

-- A thing that shuffles log messages off of running threads and logs that whenever on a dedicated one.
newtype LogQueue = LogQueue (TQueue Text)
-- TODO: Reconsider sparsity obligations. This is pretty dumb in terms of invariants we must enforce.
-- TODO: Reconsider using Map because that is honestly a retarded data structure to use here.
-- TODO: Why isn't this a newtype. This is dumb :(
-- TODO: Investigate storing just the block data (no descriptors) in the structure.
-- Maps coords to non-air blocks. BlockState 0 0 doesn't mean anything. Air just doesn't have anything in the map.
newtype ChunkSection b = ChunkSection {unChunkSection :: (Arr.Array (Nibble,Nibble,Nibble) b)} deriving (Generic)

instance Rewrapped (ChunkSection b) t
instance Wrapped (ChunkSection b) where
  type Unwrapped (ChunkSection b) = Arr.Array (Nibble,Nibble,Nibble) b

-- All the relative coords within a chunk
allCoords :: [BlockOffset]
allCoords = [BlockLocation (x,y,z) | y <- [0..15], z <- [0..15], x <- [0..15]]

emptyChunk :: ChunkSection (ForAny Block)
emptyChunk = ChunkSection $ Arr.listArray ((0,0,0),(15,15,15)) (repeat . ambiguate $ Block air Air)

chunkToWireBlock :: ChunkSection (ForAny Block) -> ChunkSection WireBlock
chunkToWireBlock = _Wrapped . mapped %~ (ambiguously toWireBlock)

-- A chunk full of air (used in chunk serialization to fill in gaps in the Map)
--airChunk :: Map BlockOffset (ForAny Block)
--airChunk = Map.fromList [(BlockLocation (x,y,z),Air) | x <- [0..15], z <- [0..15], y <- [0..15]]

data Air = Air deriving (Show,Eq)

air :: BlockDescriptor Air
air = BlockDescriptor
  {blockId = 0
  ,blockIdentifier = "minecraft:air"
  ,blockName = const "Air"
  ,blockMeta = const 0
  ,onClick = Nothing
  }


{-
-- Get the block at a location in a chunk
blockInChunk :: BlockOffset -> ChunkSection -> Some Block
blockInChunk b (ChunkSection m) = fromMaybe (some Air) (Map.lookup b m)
-}

-- Derived instance is really long (it shows all the associations of coords and
-- blocks in a giant list), so we replace it with a short placeholder
instance Show (ChunkSection b) where
  show _ = "<Chunk Section>"

-- TODO: Add paletteing. Right now, we send a full 13 bits for *every block*
-- Lights twice for (((reasons)))
instance Serial (ChunkSection WireBlock) where
  serialize chunkSection = serialize bitsPerBlock >> putWord8 0x00 >> chunkData >> lights >> lights
    where
      lights = putLazyByteString . BB.toLazyByteString . createLights . unChunkSection $ chunkSection

      -- First 9 bits are the block id, last 4 are the damage
      bitsPerBlock = 13 :: VarInt

      -- Send 0x00 as the length of the palette since we aren't using it
      --palette = BS.singleton 0x00

      -- `longChunks` is because Mojang likes to twist their data into weird shapes
      -- Might need to remove it idk
      sArray :: LBS.ByteString
      sArray = longChunks . BB.toLazyByteString . sBlockStates . unChunkSection $ chunkSection

      -- This should be the final result, but mojang is weird about chunk sections :S
      sBlockStates :: Arr.Array (Nibble,Nibble,Nibble) WireBlock -> BB.BitBuilder
      sBlockStates = foldr (\newBlock bb -> aBlock newBlock `BB.append` bb) BB.empty

      -- Annotate the data with its length in Minecraft Longs (should always be a whole number assuming 16^3 blocks/chunk)
      chunkData :: MonadPut m => m ()
      chunkData = serialize (fromIntegral (LBS.length sArray) `div` 8 :: VarInt) >> putLazyByteString sArray

      -- Right now we `const 15` the blocks, so all the blocks are fullbright
      -- TODO: Add lighting information somewhere or derive it here
      createLights = foldl' (\bb _ -> BB.fromBits 4 (15 :: Word8) `BB.append` bb) BB.empty

      -- Mojang felt like packing chunks into arrays of longs lol
      longChunks :: LBS.ByteString -> LBS.ByteString
      longChunks unchunked = LBS.concat . reverse $ bsChunksOf 8 unchunked

      -- Helper function for chunking up the BS
      bsChunksOf :: Int64 -> LBS.ByteString -> [LBS.ByteString]
      bsChunksOf x = unfoldr (\a -> if not (LBS.null a) then Just (LBS.splitAt x a) else Nothing)

      -- Serial a single block: 9 bits bid, 4 bits dmg
      aBlock :: WireBlock -> BB.BitBuilder
      aBlock (WireBlock bId meta) = BB.fromBits 9 bId `BB.append` BB.fromBits 4 meta
  deserialize = error "Unimplemented: Deserialization of Chunk Sections"

-- | A @'Block' b@ is a @b@ together with a description of how to interpret it as a Minecraft block.
data Block b = Block (BlockDescriptor b) b

-- | A @'BlockDescriptor' b@ is a description of how to use a @b@ as a block.
data BlockDescriptor b = BlockDescriptor
  {blockId :: Short 
  -- ^ The Notchian block id number of this block. Ex: @1@
  ,blockIdentifier :: String 
  -- ^ The Notchian identifier of this block. Ex @"minecraft:stone"@
  ,blockMeta :: b -> Nibble 
  -- ^ Get the Notchian block metadata value from a particular block. @const 0@ is a reasonable default. Ex @\case {Stone -> 0; Granite -> 1}@
  ,blockName :: b -> String
  -- ^ English name of this particular block. Ex @\case {Stone -> \"Stone";Granite -> \"Granite"}@
  ,onClick :: Maybe (BlockClickCallback b)
  -- ^ The callback for this block getting right-clicked
  }

-- | A callback for when a player right-clicks a block.
type BlockClickCallback b 
  =  b -- ^ The block they clicked
  -> BlockCoord -- ^ The @'BlockCoord'@ of the block
  -> BlockFace -- ^ The @'BlockFace'@ they clicked on
  -> Hand -- ^ The @'Hand'@ they clicked with
  -> (Float,Float,Float) -- ^ The part of the block they clicked
  -> Civskell ()

-- | A @'WireBlock'@ is a numerical, type-poor representation of a minecraft block
-- as a @'BlockId'@ and metadata. It is able to be directly serialized and sent over the network.
data WireBlock = WireBlock BlockId Nibble deriving (Eq,Show)

-- | Every @'Block'@ can be trivially converted to a @'WireBlock'@.
toWireBlock :: Block b -> WireBlock
toWireBlock (Block desc b) = WireBlock (blockId desc) (blockMeta desc b)

-- Note that this is an inefficient serialized normal form. 
-- See @instance 'Serial' ('ChunkSection' 'WireBlock')@ for a packed and efficient one.
instance Serial WireBlock where
  serialize (WireBlock bId meta) = serialize $ shiftL (u bId) 4 .|. (v meta .&. 0x0f)
    where
      u = unsafeCoerce :: Short -> VarInt
      v = unsafeCoerce :: Nibble -> VarInt
  deserialize = error "Unimplemented: deserialize @WireBlock"

instance Show (Block b) where
  show (Block desc b) = "Block [" ++ show (blockId desc) ++ ":" ++ show (blockMeta desc b) ++ "]"

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

{-
-- Parse an item based on its blockId, assuming no metadata and no nbt
standardParser :: forall i. Item i => i -> Parser Slot
standardParser r = do
  bid <- parseShort
  guard $ bid == itemId @i
  cnt <- anyWord8
  dmg <- parseShort
  guard $ dmg == 0
  nbtFlair <- anyWord8
  guard $ nbtFlair == 0
  return . Slot . Just $ SlotData (some r) cnt
-}
{-
placeBlock :: Block b => b -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Civskell ()
placeBlock b bc bf hand _ = do
  -- Find out what item they are trying to place
  heldSlot <- if hand == MainHand then (+36) . holdingSlot <$> getPlayer else pure 45
  getInventorySlot heldSlot >>= \case
    Slot Nothing -> loge "Fight me; this is not possible! (literally Nothing has a `placeBlock` callback)"
    Slot (Just (SlotData i icount)) -> do
      -- Remove item from inventory
      let newSlot = if icount == 1 then Slot Nothing else Slot $ Just (SlotData i (icount - 1))
      setInventorySlot heldSlot newSlot
      setBlock b (blockOnSide bc bf)
openNewWindow :: (Members '[Packeting,PlayerManipulation] r, Window w) => w -> ProtocolString -> Civskell WindowId
openNewWindow winType title = do
  wid <- usingPlayer $ \t -> do
    p <- readTVar t
    writeTVar t p {windows = Map.insert (nextWid p) (some winType) (windows p),nextWid = succ (nextWid p)}
    return (nextWid p)
  sendPacket (openWindow 0x13) (OpenWindow wid (some winType) title Nothing)
  return wid

openWindowWithItems :: (Members '[WorldManipulation,Packeting,PlayerManipulation,Logging] r,Window w) => w -> ProtocolString -> TVar Inventory -> Civskell WindowId
openWindowWithItems (ty :: tyT) name tI = do
  wid <- openNewWindow ty name
  playerInv <- Map.mapKeysMonotonic (+(27 - 9)) . playerInventory <$> getPlayer
  items <- Map.union playerInv <$> (send . WorldSTM $ readTVar tI)
  logp $ "Sending window " <> T.pack (show wid) <> " of type " <> T.pack (windowIdentifier @tyT) <> " with items " <> T.pack (show items)
  -- This is wrong because it doesn't pad between items
  sendPacket (windowItems 0x14) (WindowItems wid (ProtocolList $ Map.elems items) {-(slotCount @tyT)-})
  return wid
-}

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
{-

-- Add a new tid to the que
pendTeleport :: Members '[Packeting,Logging,PlayerManipulation] r => (Double,Double,Double) -> (Float,Float) -> Word8 -> Civskell ()
pendTeleport xyz yp relFlag = do
  p <- usingPlayer $ \t -> do
    p <- readTVar t
    writeTVar t p {nextTid = 1 + nextTid p,teleportConfirmationQue = Set.insert (nextTid p) $ teleportConfirmationQue p}
    return p
  sendPacket (playerPositionAndLook 0x2F) (PlayerPositionAndLook xyz yp relFlag (nextTid p))

-- Check if the tid is in the que. If it is, then clear and return true, else false
clearTeleport :: Member PlayerManipulation r => VarInt -> Civskell Bool
clearTeleport tid = usingPlayer $ \t -> do
  p <- readTVar t 
  writeTVar t p {teleportConfirmationQue = Set.delete tid $ teleportConfirmationQue p}
  return (Set.member tid $ teleportConfirmationQue p)

-- Set the player's gamemode
setGamemode :: Members '[Packeting,Logging,PlayerManipulation] r => Gamemode -> Civskell ()
setGamemode g = do
  overPlayer $ \p -> p {gameMode = g}
  sendPacket (changeGameState 0x1E) (ChangeGameState (ChangeGamemode g))
  case g of
    Survival -> sendPacket (playerAbilities 0x2C) (PlayerAbilities (AbilityFlags False False False False) 0 1)
    Creative -> sendPacket (playerAbilities 0x2C) (PlayerAbilities (AbilityFlags True False True True) 0 1)

-- Ideal version:
--
-- sendPacket (ChangeGameState (ChangeGamemode g))
--
-- TODO: Semantic slot descriptors
setInventorySlot :: Members '[Packeting,Logging,PlayerManipulation] r => Short -> Slot -> Civskell ()
setInventorySlot slotNum slotData = do
  overPlayer $ \p -> p {playerInventory = setSlot slotNum slotData (playerInventory p)}
  sendPacket (setSlot 0x16) (SetSlot 0 slotNum slotData)
-}

-- | @'runWorld'@ is a natural transformation from @'WorldManipulation'@ to working with actual @'STM'@ (via @'IO'@).
-- Note that this is *not* really transactional yet because it is only transactional up to the @'Get'@ or @'Put'@,
-- which is to say not transactional at all.
{-
runWorld :: Members '[Logging,IO] r => TVar WorldData -> Eff (WorldManipulation ': r) a -> Civskell a
runWorld tWorld = runNat $ \case
  Get -> send $ readTVarIO tWorld
  Put w' -> send . atomically $ writeTVar tWorld w'
-}
{-
runWorld _ (Pure x) = Pure x
runWorld w' (Eff u q) = case u of
  Inject (GetChunk chunk) -> runWorld w' . runTCQ q . Map.findWithDefault (ChunkSection Map.empty) chunk . chunks =<< send (readMVar w')
  Inject (RemoveBlock bc) -> do
    send $ modifyMVar_ w' $ \w -> do
      let f = Map.delete (blockToRelative bc)
      return w {chunks = Map.alter (Just . maybe (ChunkSection $ f Map.empty) (\(ChunkSection c) -> ChunkSection $ f c)) (blockToChunk bc) (chunks w)}
    runWorld w' $ broadcastPacket (blockChange 0x0B) (BlockChange bc (some Air))
    runWorld w' (runTCQ q ())
  Inject (SetBlock b' bc) -> do
    send $ modifyMVar_ w' $ \w -> do
      let f = Map.insert (blockToRelative bc) b'
      return w {chunks = Map.alter (Just . maybe (ChunkSection $ f Map.empty) (\(ChunkSection c) -> ChunkSection $ f c)) (blockToChunk bc) (chunks w)}
    runWorld w' $ broadcastPacket (blockChange 0x0B) (BlockChange bc b')
    runWorld w' (runTCQ q ())
  Inject (SetChunk c' cc@(ChunkCoord (x,y,z))) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = Map.insert cc c' (chunks w)}
    runWorld w' $ broadcastPacket (chunkData 0x20) (ChunkData (fromIntegral x,fromIntegral z) False (bit y) [c'] Nothing (ProtocolList []))
    runWorld w' (runTCQ q ())
  Inject (SetColumn col' (cx,cz) mBio) -> do
    send $ modifyMVar_ w' $ \w -> return w {chunks = fst $ foldl (\(m,i) c -> (Map.insert (ChunkCoord (cx,i,cz)) c m,i + 1)) (chunks w,0) col'}
    runWorld w' $ broadcastPacket (chunkData 0x20) $ chunksToColumnPacket col' (cx,cz) mBio
    runWorld w' (runTCQ q ())
  Inject FreshEID -> (>>= runWorld w' . runTCQ q) . send . modifyMVar w' $ \w -> return (w {nextEID = succ $ nextEID w},nextEID w)
  Inject FreshUUID -> (>>= runWorld w' . runTCQ q) . send . modifyMVar w' $ \w -> return (w {nextUUID = incUUID $ nextUUID w},nextUUID w)
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  -- Warning: throws error if player not found
  Inject (GetPlayer i) -> runWorld w' . runTCQ q =<< send . readTVarIO . flip (Map.!) i . players =<< send (readMVar w')
  -- Warning: throws error if player not found
  Inject (SetPlayer i p) -> do
    send . atomically . flip writeTVar p . flip (Map.!) i . players =<< send (readMVar w')
    runWorld w' (runTCQ q ())
  Inject (NewPlayer t) -> do
    pid <- runWorld w' freshEID
    send $ modifyMVar_ w' $ \w -> return w {players = Map.insert pid t $ players w}
    runWorld w' (runTCQ q pid)
  Inject (GetEntity e) -> send (readMVar w') >>= runWorld w' . runTCQ q . flip (Map.!) e . entities
  Inject (DeleteEntity e) -> send (modifyMVar_ w' $ \w -> return w {entities = Map.delete e . entities $ w}) >> runWorld w' (runTCQ q ())
  -- Pattern match on SuchThat to reinfer the constrain `Mob` into the contraint `Entity` for storage in the entities map
  Inject (SummonMob (SuchThat m)) -> do
    (eid,uuid) <- send . modifyMVar w' $ \w ->
      return (w {entities = Map.insert (nextEID w) (SuchThat m) (entities w),nextEID = succ (nextEID w),nextUUID = incUUID (nextUUID w)},(nextEID w,nextUUID w))
    runWorld w' $ broadcastPacket (spawnMob 0x03) $ makeSpawnMob eid uuid 0 (SuchThat m)
    runWorld w' (runTCQ q ())
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  Inject (SummonObject (SuchThat m)) -> do
    (eid,uuid) <- send . modifyMVar w' $ \w ->
      return (w {entities = Map.insert (nextEID w) (SuchThat m) (entities w),nextEID = succ (nextEID w),nextUUID = incUUID (nextUUID w)},(nextEID w,nextUUID w))
    runWorld w' $ broadcastPacket (spawnObject 0x00) $ SpawnObject eid uuid (SuchThat m)
    runWorld w' $ broadcastPacket (updateMetadata 0x3C) $ UpdateMetadata eid (EntityPropertySet $ map Just $ entityMeta (runIdentity m))
    runWorld w' (runTCQ q ())
    where
      incUUID (UUID (a,b)) = if b + 1 == 0 then UUID (succ a, 0) else UUID (a, succ b)
  Inject AllPlayers -> runWorld w' . runTCQ q =<< send . atomically . traverse readTVar . Map.elems . players =<< send (readMVar w')
  --Inject (ForallPlayers f) -> do
    --send $ modifyMVar_ w' $ \w -> return w {players = fmap f (players w)}
    --runWorld w' (runTCQ q ())
  Inject (BroadcastPacket pkt) -> do
    send . atomically . mapM_ (\p -> readTVar p >>= \pd -> case playerState pd of {Playing -> flip writeTQueue pkt . packetQueue $ pd; _ -> pure ()}) . players =<< send (readMVar w')
    runWorld w' (runTCQ q ())
  Inject (WorldSTM stm) -> runWorld w' . runTCQ q =<< send (atomically stm)
  Weaken u' -> Eff u' (Singleton (runWorld w' . runTCQ q))
-}

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
  ,onWindowClick :: WindowClickCallback w
  -- ^ The callback to be invoked when they click on a window of this kind
  }

-- | A callback for when a player clicks a slot in a window.
type WindowClickCallback w =
  w -- ^ The window they clicked.
  -> WindowId -- ^ The @'WindowId'@ of the window they clicked.
  -> Short -- ^ The slot number in the window they clicked.
  -> TransactionId -- ^ The @'TransactionId'@ of this click.
  -> InventoryClickMode -- ^ The manner in which they clicked the slot.
  -> WireSlot -- ^ The client provided @'WireSlot'@ that they think is in the slot they clicked.
  -> Civskell Bool -- ^ Return @'True'@ if the transaction succeeded, @'False'@ otherwise. They will be required to apologize if the transaction is rejected.

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

-- | A @'WireSlot'@ is a numerical, type-poor representation of a @'Slot'@. It has no
-- meaning or semantics until it is parsed into a proper @'Slot'@ or serialized into bytes.
data WireSlot = WireSlot (Maybe (ItemId,Word8,Short,Maybe NBT)) deriving (Eq,Show)

-- We can specify exact serialization semantics for this because it is independent of the 
-- set of actually supported items; it just represents some numbers on the wire.
-- See [the wiki.vg page](http://wiki.vg/Slot_Data) for the exact specification.
instance Serial WireSlot where
  serialize (WireSlot Nothing) = serialize @ItemId (-1)
  serialize (WireSlot (Just (iId,count,meta,mNbt))) = serialize @ItemId iId *> serialize @Word8 count *> serialize @Short meta *> serNBT mNbt
    where
      serNBT Nothing = serialize @Word8 0x00
      serNBT (Just nbt) = serialize @ProtocolNBT (ProtocolNBT nbt)
  deserialize = lookAhead (deserialize @ItemId) >>= \case
    (-1) -> pure (WireSlot Nothing)
    _ -> WireSlot . Just <$> ((,,,) <$> deserialize @ItemId <*> deserialize @Word8 <*> deserialize @Short <*> getNbt)
    where
      getNbt = lookAhead getWord8 >>= \case
        0x00 -> pure Nothing
        _ -> Just . unProtocolNBT <$> deserialize @ProtocolNBT

-- | A mapping from @'ItemId'@s to ways to construct an item of that type from
-- metadata and NBT alone (see @'ItemBuilder'@). This is sort of like a set of parsers.
type SupportedItems = Map ItemId (ForAny ItemBuilder)

-- | A way to construct an @'Item'@ from metadata and NBT. These will be paired
-- up with @'ItemId'@s from the @'Map'@ in @'SupportedItems'@.
newtype ItemBuilder i = ItemBuilder {buildItem :: Short -> Maybe NBT -> Item i}

-- | Given a set of items that are supported, and a @'WireSlot'@,
-- parse out a type-enriched @'Slot'@ of some type, or fail because we don't
-- support the provided item id.
parseItemFromSet :: SupportedItems -> WireSlot -> Maybe (ForAny Slot)
parseItemFromSet _ (WireSlot Nothing) = Just (ambiguate $ Slot Nothing)
parseItemFromSet si (WireSlot (Just (iId,count,meta,mNBT))) = case Map.lookup iId si of
  Just (SuchThat b) -> Just . ambiguate . Slot . Just $ SlotData (buildItem b meta mNBT) count
  Nothing -> Nothing

-- | Just like @'parseItemFromSet'@, but get the @'SupportedItems'@ from the @'Configuration'@'s @'supportedItems'@ field.
parseItemFromContext :: WireSlot -> Civskell (Maybe (ForAny Slot))
parseItemFromContext ws = asks configuration >>= \c -> pure $ parseItemFromSet (supportedItems c) ws

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
  ,onItemUse :: ItemUseCallback i
  -- ^ The callback for when a player right-clicks with this item.
  }

-- | A callback for when a player right-clicks with an @'Item'@. See @'onItemUse'@.
type ItemUseCallback i = Maybe (SlotData i -> Slot i,Item i -> BlockCoord -> BlockFace -> Hand -> (Float,Float,Float) -> Civskell ())

-- | Given a function to convert an item into the block to be placed, this is the standard callback for placing an item as a block.
placeBlock :: (Item i -> Block b) -> ItemUseCallback i
placeBlock f = Just (\(SlotData i ic) -> Slot . Just $ SlotData i (ic - 1),setTheBlock)
  where
    setTheBlock i bc bf _ _ = do
      wdTvar <- worldData <$> ask
      lift $ atomically $ modifyTVar wdTvar (blockInWorld (blockOnSide bc bf) .~ ambiguate (f i))

-- TODO: Reconsider sparsity obligations of Map here, and decide on a better
-- internal representation. Potentially make this abstract after all. There
-- might be a really fitting purely functional data structure to show off here.
-- | An inventory is a mapping from slot numbers to arbitrary @'Slot'@s.
type Inventory = Map.Map Short (ForAny Slot)

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

newtype ProtocolNBT = ProtocolNBT {unProtocolNBT :: NBT}

instance Serial ProtocolNBT where
  serialize = putByteString . Cereal.runPut . Cereal.put . unProtocolNBT
  deserialize = remaining >>= \r -> getByteString (fromIntegral r) >>= \bs -> case Cereal.runGet Cereal.get bs of
    Left e -> error e
    Right a -> pure (ProtocolNBT a)

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

-----------------
-- Named Types --
-----------------


-- A Nibble is a Word8 that we promise (not enforced!!!) to only use the 4 least
-- significant bits.
-- TODO: Investigate `(Bool,Bool,Bool,Bool)` or `Vect 4 Bool` as alternative.
type Nibble = Word8

-- An EncrptionCouplet holds the current internal state of the encryption
-- mechanism, including both directions' shift buffers. (Cipher,Enc,Dec)
type EncryptionCouplet = (AES128, BS.ByteString, BS.ByteString)

-- Auth infrastructure is scary, I hope it doesn't break ever :3
-- Parsed form of the result of authenticating a player with Mojang
data AuthPacket = AuthPacket UUID String [AuthProperty]

-- FromJSON instances parse JSON into AuthPackets
instance Data.Aeson.Types.FromJSON AuthPacket where
  parseJSON (Aeson.Object o) = AuthPacket <$> o .: "id" <*> o .: "name" <*> o .: "properties"
  parseJSON x = Data.Aeson.Types.typeMismatch "AuthPacket" x
-- Helper type to support the arbitrary properties in auth packets
data AuthProperty = AuthProperty ProtocolString ProtocolString (ProtocolOptional ProtocolString) deriving Generic

instance Show AuthProperty where
  show (AuthProperty (ProtocolString name) (ProtocolString value) (ProtocolOptional _mSig)) = "\"" ++ name ++ "\":\"" ++ value ++ "\""

instance Serial AuthProperty where {}

instance Data.Aeson.Types.FromJSON AuthProperty where
  parseJSON = withObject "Auth Property" $ \o -> AuthProperty <$> o .: "name" <*> o .: "value" <*> (ProtocolOptional <$> o .:? "signature")

data ClientAuthResponse = ClientAuthResponse {accessToken :: String, clientToken :: String, profileInformation :: (Maybe [ClientAuthProfile],Maybe ClientAuthProfile)} deriving Show

instance Data.Aeson.Types.FromJSON ClientAuthResponse where
  parseJSON = withObject "Client Auth Response" $ \o -> ClientAuthResponse <$> o .: "accessToken" <*> o .: "clientToken" <*> ((,) <$> o .:? "availableProfiles" <*> o .:? "selectedProfile")

data ClientAuthProfile = ClientAuthProfile {clientAuthProfileId :: String, clientAuthProfileName :: String, clientAuthProfileIsLegacy :: Bool} deriving Show

instance Data.Aeson.Types.FromJSON ClientAuthProfile where
  parseJSON = withObject "Client Auth Profile" $ \o -> ClientAuthProfile <$> o .: "id" <*> o .: "name" <*> o .:? "legacy" .!= False

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

data AsType = AsBlock | AsItem

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

-- The state of a block in the world (Block id, damage)
data BlockState = BlockState Short Word8 deriving Eq

-- Used in PlayerData to track the breaking stage of a block the player is mining.
data BlockBreak 
  = InProgress Word8 -- The block is still being broken, and has been for this many ticks.
  | DoneBreaking -- The block is done breaking and should be removed, and this entry removed.

-- Combinator for serializing Maybes in a Minecrafty way
--serOpt :: (MonadPut m,Serial s) => Maybe s -> m ()
--serOpt (Just a) = serialize True >> serialize a
--serOpt Nothing = serialize False

--deserOpt :: forall s m. (MonadGet m,Serial s) => m (Maybe s)
--deserOpt = deserialize @Bool >>= \case
 -- False -> pure Nothing
  --True -> Just <$> deserialize @s

newtype ProtocolOptional a = ProtocolOptional {unProtocolOptional :: Maybe a} deriving (Eq,Show)

instance Serial a => Serial (ProtocolOptional a) where
  serialize (ProtocolOptional ma) = case ma of
    Just a -> serialize True *> serialize a
    Nothing -> serialize False
  deserialize = deserialize @Bool >>= \case
    False -> pure (ProtocolOptional Nothing)
    True -> ProtocolOptional . Just <$> deserialize @a

newtype ProtocolString = ProtocolString {unProtocolString :: String} deriving (IsString,Eq,Data.Aeson.Types.FromJSON)

instance Show ProtocolString where
  show (ProtocolString s) = show s

instance Serial ProtocolString where
  serialize (ProtocolString str) = serialize ((fromIntegral $ BS.length encoded) :: VarInt) >> putByteString encoded
    where
      encoded = Data.ByteString.UTF8.fromString str
  deserialize = deserialize @VarInt >>= \len -> ProtocolString . Data.ByteString.UTF8.toString <$> getBytes (fromIntegral len)

data ProtocolList i a = ProtocolList {unProtocolList :: [a]}

instance Exts.IsList (ProtocolList i a) where
  type Item (ProtocolList i a) = a
  fromList = ProtocolList
  toList = unProtocolList

instance (Integral i,Serial i,Serial a) => Serial (ProtocolList i a) where
  serialize (ProtocolList xs) = serialize ((fromIntegral $ length xs) :: i) >> traverse serialize xs >> pure ()
  deserialize = deserialize @i >>= \len -> ProtocolList <$> replicateM (fromIntegral len) deserialize

-- TODO: ensure the generics instance is correct here
--instance Serial Bool where
  --serialize True = BS.singleton 0x01
  --serialize False = BS.singleton 0x00

-- Annotate a BS with its length as a VarInt
withLength :: MonadPut m => BS.ByteString -> m ()
withLength bs = serialize ((fromIntegral $ BS.length bs) :: VarInt) >> putByteString bs

-- Annotate a list of serializable things with the length of the list
withListLength :: (MonadPut m,Serial s) => [s] -> m ()
withListLength ls = serialize ((fromIntegral $ length ls) :: VarInt) >> traverse_ serialize ls

newtype LengthAnnotatedByteString = LengthAnnotatedByteString {unLengthAnnotatedByteString :: BS.ByteString} deriving (Show,Eq)

instance Serial LengthAnnotatedByteString where
  serialize (LengthAnnotatedByteString bs) = serialize @VarInt (fromIntegral $ BS.length bs) *> putByteString bs
  deserialize = LengthAnnotatedByteString <$> (getByteString . fromIntegral =<< deserialize @VarInt)

-- Entities are things with properties
class Entity m where
  entityName :: Text
  entityType :: VarInt
  entitySize :: m -> (Float,Float,Float)
  entityLocation :: m -> EntityLocation
  entityVelocity :: m -> EntityVelocity
  entityMeta :: m -> [EntityMetadata]

class Entity m => Mob m where {}

-- Some entities are Objects, which can haev special, different properties
class Entity m => Object m where
  objectName :: Text
  objectId :: Word8
  -- TODO: does objectSize always == entitySize? If so, remove objectSize
  objectSize :: m -> (Float,Float,Float)
  objectSize = entitySize
  -- TODO: does objectLocation always == entityLocation? If so, remove objectLocation
  objectLocation :: m -> EntityLocation
  objectLocation = entityLocation
  objectData :: m -> (Int32,Maybe EntityVelocity)

{-
defaultObject :: Entity m -> String -> Word8 -> (m -> (Int32,Maybe EntityVelocity)) -> Object m
defaultObject e name oId oDat = MkObject
  {entityData = e
  ,objectName = name
  ,objectId = oId
  ,objectSize = entitySize e
  ,objectLocation = entityLocation e
  ,objectData = oDat
  }
-}

-- EntityMeta is just a normal value with special type properties
newtype EntityMeta p = EntityMeta p deriving Generic

-- Those special properties are that the type has an instance for `EntityMetaType`
type EntityMetadata = SuchThat '[EntityMetaType] EntityMeta

newtype EntityPropertySet = EntityPropertySet [Maybe EntityMetadata]

-- Helper data structures to reduce reptition
data EntityLocation = EntityLocation (Double,Double,Double) (Word8,Word8) 
data EntityVelocity = EntityVelocity (Short,Short,Short)

-- EntityMeta can be Seriald in a special way, involving the `entityMetaFlag`
class Serial (EntityMeta p) => EntityMetaType p where entityMetaFlag :: Word8

-- Serializing a bunch of EntityMetadatas involves indexing, so Nothing means that is absent, but it still increments the index counter
instance Serial EntityPropertySet where
  serialize (EntityPropertySet list) = go 0 list
    where
      go :: MonadPut m => Word8 -> [Maybe EntityMetadata] -> m ()
      go idx (Just (SuchThat (EntityMeta (m::p))):xs) = putWord8 idx >> serialize (entityMetaFlag @p) >> serialize (EntityMeta m) >> go (succ idx) xs
      go idx (Nothing : xs) = go (succ idx) xs
      go _ [] = putWord8 0xff
  deserialize = error "Undefined: EntityPropertySet deserialize"

-- NYI: (4,Chat) (7,Rotation 3xFloat) (8,Position)

instance EntityMetaType Word8 where entityMetaFlag = 0x00
instance Serial (EntityMeta Word8) where {}

instance EntityMetaType VarInt where entityMetaFlag = 0x01
instance Serial (EntityMeta VarInt) where {}

instance EntityMetaType Float where entityMetaFlag = 0x02
instance Serial (EntityMeta Float) where {}

instance EntityMetaType String where entityMetaFlag = 0x03
instance Serial (EntityMeta String) where {}

instance EntityMetaType WireSlot where entityMetaFlag = 0x05
instance Serial (EntityMeta WireSlot) where {}

instance EntityMetaType Bool where entityMetaFlag = 0x06
instance Serial (EntityMeta Bool) where {}

instance EntityMetaType (ProtocolOptional BlockCoord) where entityMetaFlag = 0x09
instance Serial (EntityMeta (ProtocolOptional BlockCoord)) where {}

instance EntityMetaType BlockFace where entityMetaFlag = 0x0A
instance Serial (EntityMeta BlockFace) where 
  serialize (EntityMeta f) = serialize @VarInt $ fromIntegral (fromEnum f)
  deserialize = EntityMeta . toEnum . fromIntegral <$> deserialize @VarInt

instance EntityMetaType (ProtocolOptional UUID) where entityMetaFlag = 0x0B
instance Serial (EntityMeta (ProtocolOptional UUID)) where {}

instance EntityMetaType (Maybe BlockState) where entityMetaFlag = 0x0C
instance Serial (EntityMeta (Maybe BlockState)) where 
  serialize (EntityMeta m) = serialize @VarInt $ case m of {Nothing -> 0;Just (BlockState bid dmg) -> unsafeCoerce $ shiftL bid 4 .|. unsafeCoerce dmg}
  deserialize = deserialize @VarInt >>= \case
    0 -> pure (EntityMeta Nothing)
    x -> pure $ EntityMeta $ Just $ BlockState (unsafeCoerce $ shiftR x 4) (unsafeCoerce $ x .&. 0xf)

-- Indent the hexdump; helper function
indentedHex :: BS.ByteString -> String
indentedHex = init . unlines . map ("  "++) . lines . prettyHex

--comb :: Some c -> (forall a. c a => r) -> r
--comb (SuchThat (Identity (_ :: a))) f = f @a
