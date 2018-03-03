module Civskell.Data.Block where
import Civskell.Data.Protocol
import Data.Bytes.Serial
import Unsafe.Coerce (unsafeCoerce)
import Data.Bits

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
  --,onClick :: Maybe (BlockClickCallback b)
  -- ^ The callback for this block getting right-clicked
  }

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


