{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
module Serverbound where

import Data.Int
import Data.Word
import Data.List
import qualified Data.ByteString as BS

import Data

data Packet
  -- Handshake Packet
  -- Protocol Version, Server Address, Server Port, Next State
  = Handshake VarInt String Word16 VarInt
  -- Play Packet
  | TPConfirm VarInt
  | TabComplete
  | ChatMessage String
  | ClientStatus VarInt
  | ClientSettings String Word8 VarInt Bool Word8 VarInt
  | ConfirmTransaction
  | EnchantItem
  | ClickWindow
  | CloseWindow Word8
  | PluginMessage String BS.ByteString
  | UseEntity
  | KeepAlive VarInt
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
  | PlayerDigging VarInt BlockCoord Word8
  | EntityAction VarInt VarInt VarInt
  | SteerVehicle
  | ResourcePackStatus
  | HeldItemChange Short
  | CreativeInventoryAction Short Slot
  | UpdateSign
  | Animation VarInt
  | Spectate
  | PlayerBlockPlacement BlockCoord VarInt VarInt (Float,Float,Float)
  | UseItem VarInt
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
    (TPConfirm _) -> "TPConfirm"
    (TabComplete) -> "TabComplete"
    (ChatMessage _) -> "ChatMessage"
    (ClientStatus _) -> "ClientStatus"
    (ClientSettings _ _ _ _ _ _) -> "ClientSettings"
    (ConfirmTransaction) -> "ConfirmTransaction"
    (EnchantItem) -> "EnchantItem"
    (ClickWindow) -> "ClickWindow"
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
    (PlayerDigging _ _ _) -> "PlayerDigging"
    (EntityAction _ _ _) -> "EntityAction"
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
    (TPConfirm _) -> 0x00
    (TabComplete) -> 0x01
    (ChatMessage _) -> 0x02
    (ClientStatus _) -> 0x03
    (ClientSettings _ _ _ _ _ _) -> 0x04
    (ConfirmTransaction) -> 0x05
    (EnchantItem) -> 0x06
    (ClickWindow) -> 0x07
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
    (PlayerDigging _ _ _) -> 0x13
    (EntityAction _ _ _) -> 0x14
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
    (TPConfirm _) -> Playing
    (TabComplete) -> Playing
    (ChatMessage _) -> Playing
    (ClientStatus _) -> Playing
    (ClientSettings _ _ _ _ _ _) -> Playing
    (ConfirmTransaction) -> Playing
    (EnchantItem) -> Playing
    (ClickWindow) -> Playing
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
    (PlayerDigging _ _ _) -> Playing
    (EntityAction _ _ _) -> Playing
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

formatPacket :: String -> [(String,String)] -> String
formatPacket n [] = "{" ++ n ++ "}"
formatPacket n xs = flip (++) "]" . (++) ("{" ++ n ++ "} [") . intercalate " | " . map (\(name,val) -> if val == "" then name else name ++ ": " ++ val) $ xs
  
instance Show Packet where
  show pkt = formatPacket (packetName pkt) $ case pkt of
    (Handshake protocol addr port newstate) -> 
      [("Protocol Version",show protocol)
      ,("Server Address",addr ++ ":" ++ show port)
      ,("Requesting change to",case newstate of {1 -> "Status"; 2 -> "Login"; _ -> "Invalid"})
      ]
    (TPConfirm i) -> [("Teleport Id",show i)]
    (TabComplete) -> []
    (ChatMessage msg) -> [("Message",msg)]
    (ClientStatus status) -> [("Status",case status of {0 -> "Perform Respawn"; 1 -> "Request Stats"; 2 -> "Open Inventory"; s -> "Invalid Status (" ++ show s ++ ")"})]
    (ClientSettings loc viewDist chatMode chatColors skin hand) -> 
      [("Locale",loc)
      ,("View Distance",show viewDist)
      ,("Chat Mode",show chatMode) 
      ,("Chat colors enabled",show chatColors)
      ,("Skin bitmask",show skin)
      ,("Main Hand",show hand)
      ]
    (ConfirmTransaction) -> []
    (EnchantItem) -> []
    (ClickWindow) -> []
    (CloseWindow wid) -> [("Window Id", show wid)]
    (PluginMessage "MC|Brand" cliBrand) -> [("Client Brand",show (BS.tail cliBrand))]
    (PluginMessage _ _) -> []
    (UseEntity) -> []
    (KeepAlive i) -> [("Keep Alive Id",show i)]
    (PlayerPosition (x,y,z) grounded) -> [("Positon",show (x,y,z)),("On Ground",show grounded)]
    (PlayerPositionAndLook (x,y,z) (p,yw) grounded) -> [("Positon",show (x,y,z)),("Looking",show (p,yw)),("On Ground",show grounded)]
    (PlayerLook  (p,yw) grounded) -> [("Looking",show (p,yw)),("On Ground",show grounded)]
    (Player grounded) -> [("On Ground",show grounded)]
    (VehicleMove) -> []
    (SteerBoat) -> []
    (PlayerAbilities) -> []
    (PlayerDigging action block _side) -> [("Action",l action)] ++ if elem action [0,1,2] then [("Block",show block)] else []
      where
        l 0 = "Start Digging"
        l 1 = "Stop Digging"
        l 2 = "Finish Digging"
        l 3 = "Drop Stack"
        l 4 = "Drop Item"
        l 5 = "Special"
        l 6 = "Swap Hands"
        l _ = "Unknown action"
    (EntityAction eid action horseJump) -> [("Action",l action),("Entity Id",show eid)] ++ if elem action [5,6] then [("Horse Jump Strength",show horseJump)] else []
      where
        l 0 = "Sneak"
        l 1 = "Unsneak"
        l 2 = "Exit Bed"
        l 3 = "Sprint"
        l 4 = "Unsprint"
        l 5 = "Jump (Horse)"
        l 6 = "Unjump (Horse)"
        l 7 = "Open Inventory (Horse)"
        l 8 = "Elytra Fly"
        l _ = "Unknown action"
    (SteerVehicle) -> []
    (ResourcePackStatus) -> []
    (HeldItemChange i) -> [("Slot",show i)]
    (CreativeInventoryAction slot item) -> [("Slot",show slot),("New Item", show item)]
    (UpdateSign) -> []
    (Animation hand) -> [("Hand",case hand of {0 -> "Main"; 1 -> "Off"; h -> "Invalid hand (" ++ show h ++ ")"})]
    (Spectate) -> []
    (PlayerBlockPlacement block _side hand _cursorCoord) -> [("Block",show block),("Hand",case hand of {0 -> "Main"; 1 -> "Off"; h -> "Invalid hand (" ++ show h ++ ")"})]
    (UseItem hand) -> [("Hand",case hand of {0 -> "Main"; 1 -> "Off"; h -> "Invalid hand (" ++ show h ++ ")"})]
    (LoginStart name) -> [("Username",name)]
    (EncryptionResponse _ _) -> [("Shared Secret",""),("Verify Token","")]
    StatusRequest -> []
    (StatusPing i) -> [("Ping Id",show i)]
