{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Civskell.Packet.Serverbound where

import Data.Int
import Data.Word
import qualified Data.ByteString as BS
import Crypto.Hash (hash,Digest,SHA1)
import Numeric (showHex)

import Civskell.Data.Types hiding (Player)

  -- Protocol Version, Server Address, Server Port, Next State

data Handshake = Handshake VarInt String Word16 VarInt
instance Packet Handshake where
  type PacketSide Handshake = 'Server
  type PacketState Handshake = 'Handshaking
  packetName = "Handshake"
  packetId = 0x00
  packetPretty (Handshake protocol addr port newstate) =
    [("Protocol Version",show protocol)
    ,("Server Address",addr ++ ":" ++ show port)
    ,("Requesting change to",case newstate of {1 -> "Status"; 2 -> "Login"; _ -> "Invalid"})
    ]
  onPacket _ = return ()

data LegacyHandshake = LegacyHandshake
instance Packet LegacyHandshake where
  type PacketSide LegacyHandshake = 'Server
  type PacketState LegacyHandshake = 'Handshaking
  packetName = "LegacyHandshake"
  packetId = 0xFE
  packetPretty LegacyHandshake = []
  onPacket _ = return ()

data TPConfirm = TPConfirm VarInt
instance Packet TPConfirm where
  type PacketSide TPConfirm = 'Server
  type PacketState TPConfirm = 'Playing
  packetName = "TPConfirm"
  packetId = 0x00
  packetPretty (TPConfirm i) = [("Teleport Id",show i)]
  onPacket _ = return ()

data TabComplete = TabComplete
instance Packet TabComplete where
  type PacketSide TabComplete = 'Server
  type PacketState TabComplete = 'Playing
  packetName = "TabComplete"
  packetId = 0x01
  packetPretty (TabComplete) = []
  onPacket _ = return ()

data ChatMessage = ChatMessage String
instance Packet ChatMessage  where
  type PacketSide ChatMessage = 'Server
  type PacketState ChatMessage = 'Playing
  packetName = "ChatMessage "
  packetId = 0x02
  packetPretty (ChatMessage msg) = [("Message",msg)]
  onPacket _ = return ()

data ClientStatus = ClientStatus ClientStatusAction
instance Packet ClientStatus where
  type PacketSide ClientStatus = 'Server
  type PacketState ClientStatus = 'Playing
  packetName = "ClientStatus"
  packetId = 0x03
  packetPretty (ClientStatus status) = [("Status",show status)]
  onPacket _ = return ()

data ClientSettings = ClientSettings String Word8 VarInt Bool Word8 VarInt
instance Packet ClientSettings where
  type PacketSide ClientSettings = 'Server
  type PacketState ClientSettings = 'Playing
  packetName = "ClientSettings"
  packetId = 0x04
  packetPretty (ClientSettings loc viewDist chatMode chatColors skin hand) =
    [("Locale",loc)
    ,("View Distance",show viewDist)
    ,("Chat Mode",show chatMode)
    ,("Chat colors enabled",show chatColors)
    ,("Skin bitmask",show skin)
    ,("Main Hand",show hand)
    ]
  onPacket _ = return ()

data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool
instance Packet ConfirmTransaction where
  type PacketSide ConfirmTransaction = 'Server
  type PacketState ConfirmTransaction = 'Playing
  packetName = "ConfirmTransaction"
  packetId = 0x05
  packetPretty (ConfirmTransaction wid transId acc) = [("Window Id",show wid),("Transaction Id",show transId),("Accepted",if acc then "Yes" else "No")]
  onPacket _ = return ()

data EnchantItem = EnchantItem
instance Packet EnchantItem where
  type PacketSide EnchantItem = 'Server
  type PacketState EnchantItem = 'Playing
  packetName = "EnchantItem"
  packetId = 0x06
  packetPretty (EnchantItem) = []
  onPacket _ = return ()

-- Slot Number
data ClickWindow = ClickWindow WindowId Short TransactionId InventoryClickMode Slot
instance Packet ClickWindow where
  type PacketSide ClickWindow = 'Server
  type PacketState ClickWindow = 'Playing
  packetName = "ClickWindow"
  packetId = 0x07
  packetPretty (ClickWindow wid slotNum transId invMode item) = [("Window Id",show wid),("Slot Number",show slotNum),("Transaction Id",show transId),("Inventory Mode",show invMode),("Subject Item",show item)]
  onPacket _ = return ()

data CloseWindow = CloseWindow WindowId
instance Packet CloseWindow where
  type PacketSide CloseWindow = 'Server
  type PacketState CloseWindow = 'Playing
  packetName = "CloseWindow"
  packetId = 0x08
  packetPretty (CloseWindow wid) = [("Window Id", show wid)]
  onPacket _ = return ()

data PluginMessage = PluginMessage String BS.ByteString
instance Packet PluginMessage where
  type PacketSide PluginMessage = 'Server
  type PacketState PluginMessage = 'Playing
  packetName = "PluginMessage"
  packetId = 0x09
  packetPretty (PluginMessage "MC|Brand" cliBrand) = [("Client Brand",show (BS.tail cliBrand))]
  packetPretty (PluginMessage chan bs) = [("Channel",show chan),("Payload",show bs)]
  onPacket _ = return ()

data UseEntity = UseEntity
instance Packet UseEntity where
  type PacketSide UseEntity = 'Server
  type PacketState UseEntity = 'Playing
  packetName = "UseEntity"
  packetId = 0x0A
  packetPretty (UseEntity) = []
  onPacket _ = return ()

data KeepAlive = KeepAlive KeepAliveId
instance Packet KeepAlive where
  type PacketSide KeepAlive = 'Server
  type PacketState KeepAlive = 'Playing
  packetName = "KeepAlive"
  packetId = 0x0B
  packetPretty (KeepAlive i) = [("Keep Alive Id",show i)]
  onPacket _ = return ()

data PlayerPosition = PlayerPosition (Double,Double,Double) Bool
instance Packet PlayerPosition where
  type PacketSide PlayerPosition = 'Server
  type PacketState PlayerPosition = 'Playing
  packetName = "PlayerPosition"
  packetId = 0x0C
  packetPretty (PlayerPosition (x,y,z) grounded) = [("Positon",show (x,y,z)),("On Ground",show grounded)]
  onPacket _ = return ()

data PlayerPositionAndLook = PlayerPositionAndLook (Double,Double,Double) (Float,Float) Bool
instance Packet PlayerPositionAndLook where
  type PacketSide PlayerPositionAndLook = 'Server
  type PacketState PlayerPositionAndLook = 'Playing
  packetName = "PlayerPositionAndLook"
  packetId = 0x0D
  packetPretty (PlayerPositionAndLook (x,y,z) (yaw,pitch) grounded) = [("Positon",show (x,y,z)),("Looking",show (yaw,pitch)),("On Ground",show grounded)]
  onPacket _ = return ()

data PlayerLook = PlayerLook (Float,Float) Bool
instance Packet PlayerLook where
  type PacketSide PlayerLook = 'Server
  type PacketState PlayerLook = 'Playing
  packetName = "PlayerLook"
  packetId = 0x0E
  packetPretty (PlayerLook  (yaw,pitch) grounded) = [("Looking",show (yaw,pitch)),("On Ground",show grounded)]
  onPacket _ = return ()

data Player = Player Bool
instance Packet Player where
  type PacketSide Player = 'Server
  type PacketState Player = 'Playing
  packetName = "Player"
  packetId = 0x0F
  packetPretty (Player grounded) = [("On Ground",show grounded)]
  onPacket _ = return ()

data VehicleMove = VehicleMove
instance Packet VehicleMove where
  type PacketSide VehicleMove = 'Server
  type PacketState VehicleMove = 'Playing
  packetName = "VehicleMove"
  packetId = 0x10
  packetPretty (VehicleMove) = []
  onPacket _ = return ()

data SteerBoat = SteerBoat
instance Packet SteerBoat where
  type PacketSide SteerBoat = 'Server
  type PacketState SteerBoat = 'Playing
  packetName = "SteerBoat"
  packetId = 0x11
  packetPretty (SteerBoat) = []
  onPacket _ = return ()

data PlayerAbilities = PlayerAbilities
instance Packet PlayerAbilities where
  type PacketSide PlayerAbilities = 'Server
  type PacketState PlayerAbilities = 'Playing
  packetName = "PlayerAbilities"
  packetId = 0x12
  packetPretty (PlayerAbilities) = []
  onPacket _ = return ()

data PlayerDigging = PlayerDigging PlayerDigAction
instance Packet PlayerDigging where
  type PacketSide PlayerDigging = 'Server
  type PacketState PlayerDigging = 'Playing
  packetName = "PlayerDigging"
  packetId = 0x13
  packetPretty (PlayerDigging action) = case action of
    StartDig bc side -> [("Action","Start Digging"),("Block",show bc),("Side",show side)]
    StopDig bc side -> [("Action","Stop Digging"),("Block",show bc),("Side",show side)]
    EndDig bc side -> [("Action","Finished Digging"),("Block",show bc),("Side",show side)]
    DropItem isStack -> [("Action","Drop " ++ if isStack then "Stack" else "Item")]
    ShootArrowOrFinishEating -> [("Action","inb4 Minecraft")]
    SwapHands -> [("Action","Swap items in hands")]
  onPacket _ = return ()

data EntityAction = EntityAction PlayerId PlayerEntityAction
instance Packet EntityAction where
  type PacketSide EntityAction = 'Server
  type PacketState EntityAction = 'Playing
  packetName = "EntityAction"
  packetId = 0x14
  packetPretty (EntityAction eid fire) = (("Entity Id",show eid):) $ let u b = if b then id else ("Stop "++) in case fire of
    Sneak b -> [("Action",u b "Sneak")]
    Sprint b -> [("Action",u b "Sprint")]
    HorseJump b s -> [("Action",u b "Horse Jump"),("Horse Jump Strength",show s)]
    LeaveBed -> [("Action","Leave Bed")]
    HorseInventory -> [("Action","Open Horse Inventory")]
    ElytraFly -> [("Action","Elytra Fly")]
  onPacket _ = return ()

data SteerVehicle = SteerVehicle
instance Packet SteerVehicle where
  type PacketSide SteerVehicle = 'Server
  type PacketState SteerVehicle = 'Playing
  packetName = "SteerVehicle"
  packetId = 0x15
  packetPretty (SteerVehicle) = []
  onPacket _ = return ()

data ResourcePackStatus = ResourcePackStatus
instance Packet ResourcePackStatus where
  type PacketSide ResourcePackStatus = 'Server
  type PacketState ResourcePackStatus = 'Playing
  packetName = "ResourcePackStatus"
  packetId = 0x16
  packetPretty (ResourcePackStatus) = []
  onPacket _ = return ()

data HeldItemChange = HeldItemChange Short
instance Packet HeldItemChange where
  type PacketSide HeldItemChange = 'Server
  type PacketState HeldItemChange = 'Playing
  packetName = "HeldItemChange"
  packetId = 0x17
  packetPretty (HeldItemChange i) = [("Slot",show i)]
  onPacket _ = return ()

data CreativeInventoryAction = CreativeInventoryAction Short Slot
instance Packet CreativeInventoryAction where
  type PacketSide CreativeInventoryAction = 'Server
  type PacketState CreativeInventoryAction = 'Playing
  packetName = "CreativeInventoryAction"
  packetId = 0x18
  packetPretty (CreativeInventoryAction slot item) = [("Slot",show slot),("New Item", show item)]
  onPacket _ = return ()

data UpdateSign = UpdateSign
instance Packet UpdateSign where
  type PacketSide UpdateSign = 'Server
  type PacketState UpdateSign = 'Playing
  packetName = "UpdateSign"
  packetId = 0x19
  packetPretty (UpdateSign) = []
  onPacket _ = return ()

data Animation = Animation Hand
instance Packet Animation where
  type PacketSide Animation = 'Server
  type PacketState Animation = 'Playing
  packetName = "Animation"
  packetId = 0x1A
  packetPretty (Animation hand) = [("Hand",show hand)]
  onPacket _ = return ()

data Spectate = Spectate
instance Packet Spectate where
  type PacketSide Spectate = 'Server
  type PacketState Spectate = 'Playing
  packetName = "Spectate"
  packetId = 0x1B
  packetPretty (Spectate) = []
  onPacket _ = return ()

data PlayerBlockPlacement = PlayerBlockPlacement BlockCoord BlockFace Hand (Float,Float,Float)
instance Packet PlayerBlockPlacement where
  type PacketSide PlayerBlockPlacement = 'Server
  type PacketState PlayerBlockPlacement = 'Playing
  packetName = "PlayerBlockPlacement"
  packetId = 0x1C
  packetPretty (PlayerBlockPlacement block _side hand _cursorCoord) = [("Block",show block),("Hand",show hand)]
  onPacket _ = return ()

data UseItem = UseItem Hand
instance Packet UseItem where
  type PacketSide UseItem = 'Server
  type PacketState UseItem = 'Playing
  packetName = "UseItem"
  packetId = 0x1D
  packetPretty (UseItem hand) = [("Hand",show hand)]
  onPacket _ = return ()

data LoginStart = LoginStart String
instance Packet LoginStart where
  type PacketSide LoginStart = 'Server
  type PacketState LoginStart = 'LoggingIn
  packetName = "LoginStart"
  packetId = 0x00
  packetPretty (LoginStart name) = [("Username",name)]
  onPacket _ = return ()

data EncryptionResponse = EncryptionResponse BS.ByteString BS.ByteString
instance Packet EncryptionResponse where
  type PacketSide EncryptionResponse = 'Server
  type PacketState EncryptionResponse = 'LoggingIn
  packetName = "EncryptionResponse"
  packetId = 0x01
  packetPretty (EncryptionResponse ss vt) = 
    [("Shared Secret Hash",(take 7 $ show (hash ss :: Digest SHA1)) ++ "...")
    ,("Verify Token Hash",(take 7 $ show (hash vt :: Digest SHA1)) ++ "...")
    ]
  onPacket _ = return ()

data StatusRequest = StatusRequest
instance Packet StatusRequest where
  type PacketSide StatusRequest = 'Server
  type PacketState StatusRequest = 'Status
  packetName = "StatusRequest"
  packetId = 0x00
  packetPretty StatusRequest = []
  onPacket _ = return ()

data StatusPing = StatusPing Int64
instance Packet StatusPing where
  type PacketSide StatusPing = 'Server
  type PacketState StatusPing = 'Status
  packetName = "StatusPing"
  packetId = 0x01
  packetPretty (StatusPing i) = [("Ping Token","0x" ++ showHex i "")]
  onPacket _ = return ()

