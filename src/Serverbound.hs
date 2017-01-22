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
  | TPConfirm
  | TabComplete
  | ChatMessage
  | ClientStatus
  | ClientSettings
  | ConfirmTransaction
  | EnchantItem
  | ClickWindow
  | CloseWindow
  | PluginMessage String BS.ByteString
  | UseEntity
  | KeepAlive
  | PlayerPosition
  | PlayerPositionAndLook
  | PlayerLook
  | Player
  | VehicleMove
  | SteerBoat
  | PlayerAbilities
  | PlayerDigging
  | EntityAction
  | SteerVehicle
  | ResourcePackStatus
  | HeldItemChange
  | CreativeInventoryAction
  | UpdateSign
  | Animation
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

instance PacketId Packet where
  packetSide _ = Server
  packetId p = case p of
    (Handshake _ _ _ _) -> 0x00
    (TPConfirm) -> 0x00
    (TabComplete) -> 0x01
    (ChatMessage) -> 0x02
    (ClientStatus) -> 0x03
    (ClientSettings) -> 0x04
    (ConfirmTransaction) -> 0x05
    (EnchantItem) -> 0x06
    (ClickWindow) -> 0x07
    (CloseWindow) -> 0x08
    (PluginMessage _ _) -> 0x09
    (UseEntity) -> 0x0A
    (KeepAlive) -> 0x0B
    (PlayerPosition) -> 0x0C
    (PlayerPositionAndLook) -> 0x0D
    (PlayerLook) -> 0x0E
    (Player) -> 0x0F
    (VehicleMove) -> 0x10
    (SteerBoat) -> 0x11
    (PlayerAbilities) -> 0x12
    (PlayerDigging) -> 0x13
    (EntityAction) -> 0x14
    (SteerVehicle) -> 0x15
    (ResourcePackStatus) -> 0x16
    (HeldItemChange) -> 0x17
    (CreativeInventoryAction) -> 0x18
    (UpdateSign) -> 0x19
    (Animation) -> 0x1A
    (Spectate) -> 0x1B
    (PlayerBlockPlacement) -> 0x1C
    (UseItem) -> 0x1D
    (LoginStart _) -> 0x00
    (EncryptionResponse _ _) -> 0x01
    StatusRequest -> 0x00
    (StatusPing _) -> 0x01
  packetState p = case p of
    (Handshake _ _ _ _) -> Handshaking
    (TPConfirm) -> Playing
    (TabComplete) -> Playing
    (ChatMessage) -> Playing
    (ClientStatus) -> Playing
    (ClientSettings) -> Playing
    (ConfirmTransaction) -> Playing
    (EnchantItem) -> Playing
    (ClickWindow) -> Playing
    (CloseWindow) -> Playing
    (PluginMessage _ _) -> Playing
    (UseEntity) -> Playing
    (KeepAlive) -> Playing
    (PlayerPosition) -> Playing
    (PlayerPositionAndLook) -> Playing
    (PlayerLook) -> Playing
    (Player) -> Playing
    (VehicleMove) -> Playing
    (SteerBoat) -> Playing
    (PlayerAbilities) -> Playing
    (PlayerDigging) -> Playing
    (EntityAction) -> Playing
    (SteerVehicle) -> Playing
    (ResourcePackStatus) -> Playing
    (HeldItemChange) -> Playing
    (CreativeInventoryAction) -> Playing
    (UpdateSign) -> Playing
    (Animation) -> Playing
    (Spectate) -> Playing
    (PlayerBlockPlacement) -> Playing
    (UseItem) -> Playing
    (LoginStart _) -> LoggingIn
    (EncryptionResponse _ _) -> LoggingIn
    StatusRequest -> Status
    (StatusPing _) -> Status
  
