{-# LANGUAGE TemplateHaskell #-}
module Civskell.Data.World where
import Civskell.Data.Block
import Civskell.Data.Chunk
import Civskell.Data.Protocol
import Civskell.Data.Common
import Civskell.Data.Entity
import Civskell.Data.Player
import Control.Concurrent.STM
import Data.Map.Lazy (Map)
import Data.SuchThat
import Control.Lens
-- All the information about a mineman world. Notably, players is a Map of PId's to TVars of player data, not actual player data
data WorldData = WorldData 
  {_worldChunks :: Map ChunkCoord (ChunkSection (ForAny Block))
  ,_worldGenerator :: WorldGenerator ()
  ,_worldEntities :: Map EntityId (Satisfies Entity)
  ,_worldPlayers :: Map PlayerId (TVar PlayerData)
  ,_worldNextEID :: EntityId
  ,_worldNextUUID :: UUID
  }

-- | A @'WorldGenerator' 's'@ generates chunks given a seed 's'.
newtype WorldGenerator s = WorldGenerator (s -> ChunkCoord -> ChunkSection (ForAny Block))

emptyWorldGenerator :: WorldGenerator ()
emptyWorldGenerator = WorldGenerator $ \() _ -> emptyChunk


makeLenses ''WorldData


