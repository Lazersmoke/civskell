{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
module Civskell.Packet.Serverbound where

import Data.Int
import Data.Word
import qualified Data.ByteString as BS
import Crypto.Hash (hash,Digest,SHA1)
import Numeric (showHex)

import Civskell.Data.Types

data Packet
  -- Handshake Packet
  -- Protocol Version, Server Address, Server Port, Next State
  = Handshake VarInt String Word16 VarInt
  | LegacyHandshake
  -- Play Packet
  | TPConfirm VarInt
  | TabComplete
  | ChatMessage String
  | ClientStatus ClientStatusAction
  | ClientSettings String Word8 VarInt Bool Word8 VarInt
  -- Transaction accpeted?
  | ConfirmTransaction WindowId TransactionId Bool
  | EnchantItem
  -- Slot Number
  | ClickWindow WindowId Short TransactionId InventoryClickMode Slot
  | CloseWindow WindowId
  | PluginMessage String BS.ByteString
  | UseEntity
  | KeepAlive KeepAliveId
  -- x,y,z (abs), on ground
  | PlayerPosition (Double,Double,Double) Bool
  -- x,y,z, yaw,pitch, onground
  | PlayerPositionAndLook (Double,Double,Double) (Float,Float) Bool
  -- yaw,pitch, onground
  | PlayerLook (Float,Float) Bool
  -- onground
  | Player Bool
  | VehicleMove
  | SteerBoat
  | PlayerAbilities
  | PlayerDigging PlayerDigAction
  | EntityAction PlayerId PlayerEntityAction
  | SteerVehicle
  | ResourcePackStatus
  | HeldItemChange Short
  | CreativeInventoryAction Short Slot
  | UpdateSign
  | Animation Hand
  | Spectate
  | PlayerBlockPlacement BlockCoord BlockFace Hand (Float,Float,Float)
  | UseItem Hand
  -- Login Packet
  -- Username (apparently)
  | LoginStart String
  -- Shared Secret Length, Shared Secret, Verify Token Length, Verify Token
  | EncryptionResponse BS.ByteString BS.ByteString
  -- Status Packet
  -- Nullary
  | StatusRequest
  -- Arbitrary Number (see Clientbound.StatusPong)
  | StatusPing Int64

--parseServerboundPacket :: Word8 -> Parser Packet
--parseServerboundPacket pktId = try (specificVarInt pktId)

instance PacketId Packet where
  packetSide _ = Server
  packetName p = case p of
    (Handshake _ _ _ _) -> "Handshake"
    LegacyHandshake -> "LegacyHandshake"
    (TPConfirm _) -> "TPConfirm"
    (TabComplete) -> "TabComplete"
    (ChatMessage _) -> "ChatMessage"
    (ClientStatus _) -> "ClientStatus"
    (ClientSettings _ _ _ _ _ _) -> "ClientSettings"
    (ConfirmTransaction _ _ _) -> "ConfirmTransaction"
    (EnchantItem) -> "EnchantItem"
    (ClickWindow _ _ _ _ _) -> "ClickWindow"
    (CloseWindow _) -> "CloseWindow"
    (PluginMessage _ _) -> "PluginMessage"
    (UseEntity) -> "UseEntity"
    (KeepAlive _) -> "KeepAlive"
    (PlayerPosition _ _) -> "PlayerPosition"
    (PlayerPositionAndLook _ _ _) -> "PlayerPositionAndLook"
    (PlayerLook _ _) -> "PlayerLook"
    (Player _) -> "Player"
    (VehicleMove) -> "VehicleMove"
    (SteerBoat) -> "SteerBoat"
    (PlayerAbilities) -> "PlayerAbilities"
    (PlayerDigging _) -> "PlayerDigging"
    (EntityAction _ _) -> "EntityAction"
    (SteerVehicle) -> "SteerVehicle"
    (ResourcePackStatus) -> "ResourcePackStatus"
    (HeldItemChange _) -> "HeldItemChange"
    (CreativeInventoryAction _ _) -> "CreativeInventoryAction"
    (UpdateSign) -> "UpdateSign"
    (Animation _) -> "Animation"
    (Spectate) -> "Spectate"
    (PlayerBlockPlacement _ _ _ _) -> "PlayerBlockPlacement"
    (UseItem _) -> "UseItem"
    (LoginStart _) -> "LoginStart"
    (EncryptionResponse _ _) -> "EncryptionResponse"
    StatusRequest -> "StatusRequest"
    (StatusPing _) -> "StatusPing"
  packetId p = case p of
    (Handshake _ _ _ _) -> 0x00
    LegacyHandshake -> 0xFE
    (TPConfirm _) -> 0x00
    (TabComplete) -> 0x01
    (ChatMessage _) -> 0x02
    (ClientStatus _) -> 0x03
    (ClientSettings _ _ _ _ _ _) -> 0x04
    (ConfirmTransaction _ _ _) -> 0x05
    (EnchantItem) -> 0x06
    (ClickWindow _ _ _ _ _) -> 0x07
    (CloseWindow _) -> 0x08
    (PluginMessage _ _) -> 0x09
    (UseEntity) -> 0x0A
    (KeepAlive _) -> 0x0B
    (PlayerPosition _ _) -> 0x0C
    (PlayerPositionAndLook _ _ _) -> 0x0D
    (PlayerLook _ _) -> 0x0E
    (Player _) -> 0x0F
    (VehicleMove) -> 0x10
    (SteerBoat) -> 0x11
    (PlayerAbilities) -> 0x12
    (PlayerDigging _) -> 0x13
    (EntityAction _ _) -> 0x14
    (SteerVehicle) -> 0x15
    (ResourcePackStatus) -> 0x16
    (HeldItemChange _) -> 0x17
    (CreativeInventoryAction _ _) -> 0x18
    (UpdateSign) -> 0x19
    (Animation _) -> 0x1A
    (Spectate) -> 0x1B
    (PlayerBlockPlacement _ _ _ _) -> 0x1C
    (UseItem _) -> 0x1D
    (LoginStart _) -> 0x00
    (EncryptionResponse _ _) -> 0x01
    StatusRequest -> 0x00
    (StatusPing _) -> 0x01
  packetState p = case p of
    (Handshake _ _ _ _) -> Handshaking
    LegacyHandshake -> Handshaking
    (TPConfirm _) -> Playing
    (TabComplete) -> Playing
    (ChatMessage _) -> Playing
    (ClientStatus _) -> Playing
    (ClientSettings _ _ _ _ _ _) -> Playing
    (ConfirmTransaction _ _ _) -> Playing
    (EnchantItem) -> Playing
    (ClickWindow _ _ _ _ _) -> Playing
    (CloseWindow _) -> Playing
    (PluginMessage _ _) -> Playing
    (UseEntity) -> Playing
    (KeepAlive _) -> Playing
    (PlayerPosition _ _) -> Playing
    (PlayerPositionAndLook _ _ _) -> Playing
    (PlayerLook _ _) -> Playing
    (Player _) -> Playing
    (VehicleMove) -> Playing
    (SteerBoat) -> Playing
    (PlayerAbilities) -> Playing
    (PlayerDigging _) -> Playing
    (EntityAction _ _) -> Playing
    (SteerVehicle) -> Playing
    (ResourcePackStatus) -> Playing
    (HeldItemChange _) -> Playing
    (CreativeInventoryAction _ _) -> Playing
    (UpdateSign) -> Playing
    (Animation _) -> Playing
    (Spectate) -> Playing
    (PlayerBlockPlacement _ _ _ _) -> Playing
    (UseItem _) -> Playing
    (LoginStart _) -> LoggingIn
    (EncryptionResponse _ _) -> LoggingIn
    StatusRequest -> Status
    (StatusPing _) -> Status

-- {Handshake} Protocol Version: 210 | Server Address: localhost:8989 | Change to: Login

instance Show Packet where
  show pkt = formatPacket (packetName pkt) $ case pkt of
    (Handshake protocol addr port newstate) ->
      [("Protocol Version",show protocol)
      ,("Server Address",addr ++ ":" ++ show port)
      ,("Requesting change to",case newstate of {1 -> "Status"; 2 -> "Login"; _ -> "Invalid"})
      ]
    LegacyHandshake -> []
    (TPConfirm i) -> [("Teleport Id",show i)]
    (TabComplete) -> []
    (ChatMessage msg) -> [("Message",msg)]
    (ClientStatus status) -> [("Status",show status)]
    (ClientSettings loc viewDist chatMode chatColors skin hand) ->
      [("Locale",loc)
      ,("View Distance",show viewDist)
      ,("Chat Mode",show chatMode)
      ,("Chat colors enabled",show chatColors)
      ,("Skin bitmask",show skin)
      ,("Main Hand",show hand)
      ]
    (ConfirmTransaction wid transId acc) -> [("Window Id",show wid),("Transaction Id",show transId),("Accepted",if acc then "Yes" else "No")]
    (EnchantItem) -> []
    (ClickWindow wid slotNum transId invMode item) -> [("Window Id",show wid),("Slot Number",show slotNum),("Transaction Id",show transId),("Inventory Mode",show invMode),("Subject Item",show item)]
    (CloseWindow wid) -> [("Window Id", show wid)]
    (PluginMessage "MC|Brand" cliBrand) -> [("Client Brand",show (BS.tail cliBrand))]
    (PluginMessage chan bs) -> [("Channel",show chan),("Payload",show bs)]
    (UseEntity) -> []
    (KeepAlive i) -> [("Keep Alive Id",show i)]
    (PlayerPosition (x,y,z) grounded) -> [("Positon",show (x,y,z)),("On Ground",show grounded)]
    (PlayerPositionAndLook (x,y,z) (yaw,pitch) grounded) -> [("Positon",show (x,y,z)),("Looking",show (yaw,pitch)),("On Ground",show grounded)]
    (PlayerLook  (yaw,pitch) grounded) -> [("Looking",show (yaw,pitch)),("On Ground",show grounded)]
    (Player grounded) -> [("On Ground",show grounded)]
    (VehicleMove) -> []
    (SteerBoat) -> []
    (PlayerAbilities) -> []
    (PlayerDigging action) -> case action of
      StartDig bc side -> [("Action","Start Digging"),("Block",show bc),("Side",show side)]
      StopDig bc side -> [("Action","Stop Digging"),("Block",show bc),("Side",show side)]
      EndDig bc side -> [("Action","Finished Digging"),("Block",show bc),("Side",show side)]
      DropItem isStack -> [("Action","Drop " ++ if isStack then "Stack" else "Item")]
      ShootArrowOrFinishEating -> [("Action","inb4 Minecraft")]
      SwapHands -> [("Action","Swap items in hands")]
    (EntityAction eid action) -> (("Entity Id",show eid):) $ let u b = if b then id else ("Stop "++) in case action of
      Sneak b -> [("Action",u b "Sneak")]
      Sprint b -> [("Action",u b "Sprint")]
      HorseJump b s -> [("Action",u b "Horse Jump"),("Horse Jump Strength",show s)]
      LeaveBed -> [("Action","Leave Bed")]
      HorseInventory -> [("Action","Open Horse Inventory")]
      ElytraFly -> [("Action","Elytra Fly")]
    (SteerVehicle) -> []
    (ResourcePackStatus) -> []
    (HeldItemChange i) -> [("Slot",show i)]
    (CreativeInventoryAction slot item) -> [("Slot",show slot),("New Item", show item)]
    (UpdateSign) -> []
    (Animation hand) -> [("Hand",show hand)]
    (Spectate) -> []
    (PlayerBlockPlacement block _side hand _cursorCoord) -> [("Block",show block),("Hand",show hand)]
    (UseItem hand) -> [("Hand",show hand)]
    (LoginStart name) -> [("Username",name)]
    (EncryptionResponse ss vt) -> 
      [("Shared Secret Hash",(take 7 $ show (hash ss :: Digest SHA1)) ++ "...")
      ,("Verify Token Hash",(take 7 $ show (hash vt :: Digest SHA1)) ++ "...")
      ]
    StatusRequest -> []
    (StatusPing i) -> [("Ping Token","0x" ++ showHex i "")]
