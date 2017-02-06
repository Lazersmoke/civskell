{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
module Serverbound where

import Data.Int
import Data.Word
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
  | PlayerPosition (Position Double) Bool
  -- x,y,z, yaw,pitch, onground
  | PlayerPositionAndLook (Position Double) (Float,Float) Bool
  -- yaw,pitch, onground
  | PlayerLook (Float,Float) Bool
  -- onground
  | Player Bool
  | VehicleMove
  | SteerBoat
  | PlayerAbilities
  | PlayerDigging
  | EntityAction
  | SteerVehicle
  | ResourcePackStatus
  | HeldItemChange Short
  | CreativeInventoryAction Short Slot
  | UpdateSign
  | Animation VarInt
  | Spectate
  | PlayerBlockPlacement
  | UseItem
  -- Login Packet
  -- Username (apparently)
  | LoginStart String
  -- Shared Secret Length, Shared Secret, Verify Token Length, Verify Token
  | EncryptionResponse BS.ByteString BS.ByteString
  -- Status Packet
  -- Nullary
  | StatusRequest
  -- Arbitrary Number (see Clientbound.StatusPong)
  | StatusPing Int64 deriving Show

--parseServerboundPacket :: Word8 -> Parser Packet
--parseServerboundPacket pktId = try (specificVarInt pktId)

instance PacketId Packet where
  packetSide _ = Server
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
    (PlayerDigging) -> 0x13
    (EntityAction) -> 0x14
    (SteerVehicle) -> 0x15
    (ResourcePackStatus) -> 0x16
    (HeldItemChange _) -> 0x17
    (CreativeInventoryAction _ _) -> 0x18
    (UpdateSign) -> 0x19
    (Animation _) -> 0x1A
    (Spectate) -> 0x1B
    (PlayerBlockPlacement) -> 0x1C
    (UseItem) -> 0x1D
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
    (PlayerDigging) -> Playing
    (EntityAction) -> Playing
    (SteerVehicle) -> Playing
    (ResourcePackStatus) -> Playing
    (HeldItemChange _) -> Playing
    (CreativeInventoryAction _ _) -> Playing
    (UpdateSign) -> Playing
    (Animation _) -> Playing
    (Spectate) -> Playing
    (PlayerBlockPlacement) -> Playing
    (UseItem) -> Playing
    (LoginStart _) -> LoggingIn
    (EncryptionResponse _ _) -> LoggingIn
    StatusRequest -> Status
    (StatusPing _) -> Status
  
