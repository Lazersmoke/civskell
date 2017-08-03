{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Civskell.Versions.Vanilla where

import qualified Civskell.Packet.Serverbound as Server
import Civskell.Data.Types

import qualified Data.Vector as Vector
import Data.SuchThat

vanilla1_12 :: Configuration
vanilla1_12 = defaultConfiguration
  {packetsForState = \case
    Handshaking -> Vector.fromList 
      [ambiguate Server.handshake
      ,ambiguate Server.legacyHandshake
      ]
    LoggingIn -> Vector.fromList 
      [ambiguate Server.loginStart
      ,ambiguate Server.encryptionResponse
      ]
    Status -> Vector.fromList 
      [ambiguate Server.statusRequest
      ,ambiguate Server.statusPing
      ]
    Playing -> Vector.fromList
      [ambiguate Server.tpConfirm
      ,ambiguate Server.prepareCraftingGrid
      ,ambiguate Server.tabComplete
      ,ambiguate Server.chatMessage
      ,ambiguate Server.clientStatus
      ,ambiguate Server.clientSettings
      ,ambiguate Server.confirmTransaction
      ,ambiguate Server.enchantItem
      ,ambiguate Server.clickWindow
      ,ambiguate Server.closeWindow
      ,ambiguate Server.pluginMessage
      ,ambiguate Server.useEntity
      ,ambiguate Server.keepAlive
      ,ambiguate Server.player
      ,ambiguate Server.playerPosition
      ,ambiguate Server.playerPositionAndLook
      ,ambiguate Server.playerLook
      ,ambiguate Server.vehicleMove
      ,ambiguate Server.steerBoat
      ,ambiguate Server.playerAbilities
      ,ambiguate Server.playerDigging
      ,ambiguate Server.entityAction
      ,ambiguate Server.steerVehicle
      ,ambiguate Server.craftingBookData
      ,ambiguate Server.resourcePackStatus
      ,ambiguate Server.advancementTab
      ,ambiguate Server.heldItemChange
      ,ambiguate Server.creativeInventoryAction
      ,ambiguate Server.updateSign
      ,ambiguate Server.animation
      ,ambiguate Server.playerBlockPlacement
      ,ambiguate Server.useItem
      ]
  ,serverVersion = "Vanilla 1.12"
  ,protocolVersion = 335
  }

vanilla1_12_1 :: Configuration
vanilla1_12_1 = vanilla1_12
  {packetsForState = \case
    Playing -> moveVec 0x01 0x12 (packetsForState vanilla1_12 Playing)
    x -> packetsForState vanilla1_12 x
  ,serverVersion = "Vanilla 1.12.1"
  ,protocolVersion = 338
  }
