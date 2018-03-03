{-# LANGUAGE TemplateHaskell #-}
module Civskell.Data.Player where
import Civskell.Data.Protocol
import Civskell.Data.Common
import Civskell.Data.Item
import Control.Concurrent.STM
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as Vector
import qualified Data.Set as Set
import Data.SuchThat
import Data.Word
import Control.Lens
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
-- | Used in PlayerData to track the breaking stage of a block the player is mining.
data BlockBreak 
  = InProgress Word8 -- ^ The block is still being broken, and has been for this many ticks.
  | DoneBreaking -- ^ The block is done breaking and should be removed, and this entry removed.


-- | An inventory is a mapping from slot numbers to arbitrary @'Slot'@s.
type Inventory = Vector.Vector (ForAny Slot)

makeLenses ''PlayerData


