{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Configurations for running Civskell with various minecraft protocols
module Civskell.Versions.Protocol where

import qualified Civskell.Packet.Serverbound as Server
import qualified Civskell.Packet.Clientbound as Client
import Civskell.Data.Types
import Civskell.Versions.Vanilla

import qualified Data.Vector as Vector
import Data.SuchThat
import Data.Int (Int64)

-- | @'protocol340'@ is the same as @'protocol339'@.
protocol340 :: Configuration
protocol340 = protocol339
  {serverVersion = "Vanilla 1.12.2"
  ,protocolVersion = 340
  }

-- | @'protocol339'@ as a tweak to @'protocol338'@.
protocol339 :: Configuration
protocol339 = protocol338
  {serverVersion = "Vanilla 1.12.1-pre1, -pre2"
  ,protocolVersion = 339
  ,packetsForState = \case
    Playing -> packetsForState protocol338 Playing Vector.//
      -- Keep alives now use longs instead of VarInts
      [(0x0B,ambiguate $ Server.keepAlive @Int64 $ handleKeepAlive)
      ]
    x -> packetsForState protocol338 x
  ,keepAliveThread = repeatedly 0 (keepAliveAction $ \i -> DescribedPacket (Client.keepAlive @Int64 0x1F) (Client.KeepAlive i)) 20000000
  }

-- | Civskell configuration for running @'protocol338'@.
protocol338 :: Configuration
protocol338  = defaultConfiguration
  {packetsForState = \case
    Handshaking -> Vector.fromList 
      [ambiguate $ Server.handshake $ handleHandshake
      ,ambiguate $ Server.legacyHandshake $ handleLegacyHandshake
      ]
    LoggingIn -> Vector.fromList 
      [ambiguate $ Server.loginStart $ handleLoginStart
      ,ambiguate $ Server.encryptionResponse $ handleEncryptionResponse
      ]
    Status -> Vector.fromList 
      [ambiguate $ Server.statusRequest $ handleStatusRequest
      ,ambiguate $ Server.statusPing $ handleStatusPing
      ]
    Playing -> Vector.fromList
      [ambiguate $ Server.tpConfirm $ handleTPConfirm
      ,ambiguate $ Server.tabComplete $ Server.unhandled "Tab Complete"
      ,ambiguate $ Server.chatMessage $ handleChatMessage
      ,ambiguate $ Server.clientStatus $ handleClientStatus
      ,ambiguate $ Server.clientSettings $ handleClientSettings
      ,ambiguate $ Server.confirmTransaction $ handleConfirmTransaction
      ,ambiguate $ Server.enchantItem $ Server.unhandled "Enchant Item"
      ,ambiguate $ Server.clickWindow $ handleClickWindow
      ,ambiguate $ Server.closeWindow $ handleCloseWindow
      ,ambiguate $ Server.pluginMessage $ handlePluginMessage
      ,ambiguate $ Server.useEntity $ handleUseEntity
      ,ambiguate $ Server.keepAlive @VarInt $ handleKeepAlive
      ,ambiguate $ Server.player $ handlePlayer
      ,ambiguate $ Server.playerPosition $ handlePlayerPosition
      ,ambiguate $ Server.playerPositionAndLook $ handlePlayerPositionAndLook
      ,ambiguate $ Server.playerLook $ handlePlayerLook
      ,ambiguate $ Server.vehicleMove $ Server.unhandled "Vehicle Move"
      ,ambiguate $ Server.steerBoat $ Server.unhandled "Steer Boat"
      ,ambiguate $ Server.craftRecipeRequest $ Server.unhandled "Craft Recipe Request"
      -- Only sent when flight is toggled
      ,ambiguate $ Server.playerAbilities $ handlePlayerAbilities
      ,ambiguate $ Server.playerDigging $ handlePlayerDigging
      ,ambiguate $ Server.entityAction $ handleEntityAction
      ,ambiguate $ Server.steerVehicle $ Server.unhandled "Steer Vehicle"
      ,ambiguate $ Server.craftingBookData $ Server.unhandled "Crafting Book Data"
      ,ambiguate $ Server.resourcePackStatus $ Server.unhandled "Resource Pack Status"
      ,ambiguate $ Server.advancementTab $ Server.unhandled "Advancement Tab"
      ,ambiguate $ Server.heldItemChange $ handleHeldItemChange
      ,ambiguate $ Server.creativeInventoryAction $ handleCreativeInventoryAction
      ,ambiguate $ Server.updateSign $ Server.unhandled "Update Sign"
      ,ambiguate $ Server.animation $ handleAnimation
      ,ambiguate $ Server.spectate $ Server.unhandled "Spectate"
      ,ambiguate $ Server.playerBlockPlacement $ handlePlayerBlockPlacement
      ,ambiguate $ Server.useItem $ handleUseItem
      ]
  ,serverVersion = "Vanilla 1.12.1"
  ,protocolVersion = 338
  ,keepAliveThread = repeatedly 0 (keepAliveAction $ \i -> DescribedPacket (Client.keepAlive @VarInt 0x1F) (Client.KeepAlive i)) 20000000
  }
