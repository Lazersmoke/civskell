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
import Data.Int
import Data.Word

-- | @'protocol340'@ is the same as @'protocol339'@.
protocol340 :: Configuration
protocol340 = protocol339
  {serverVersion = "Vanilla 1.12.2"
  ,protocolVersion = 340
  }

-- | @'protocol339'@ is @'protocol338'@, but with @'Int64'@ keep alive ids.
protocol339 :: Configuration
protocol339 = protocol338
  {serverVersion = "Vanilla 1.12.2-pre1, -pre2"
  ,protocolVersion = 339
  ,packetsForState = \case
    Playing -> packetsForState protocol338 Playing Vector.//
      -- Keep alives now use longs instead of VarInts
      [(0x0B,ambiguate $ Server.keepAlive @Int64 $ handleKeepAlive)
      ]
    x -> packetsForState protocol338 x
  ,keepAliveThread = repeatedly 0 (keepAliveAction $ \i -> DescribedPacket (Client.keepAlive @Int64 0x1F) (Client.KeepAlive i)) 20000000
  }

-- | @'protcol338'@ is the same as @'protocol337'@.
protocol338 :: Configuration
protocol338 = protocol337
  {serverVersion = "Vanilla 1.12.1"
  ,protocolVersion = 338
  }

-- | @'protcol337'@ is the same as @'protocol336'@.
protocol337 :: Configuration
protocol337 = protocol336
  {serverVersion = "Vanilla 1.12.1-pre1"
  ,protocolVersion = 337
  }

-- | @'protcol336'@ is @'protocol335'@ but with a new craft recipe response packet.
protocol336 :: Configuration
protocol336 = protocol335
  {serverVersion = "Vanilla 17w31a"
  ,protocolVersion = 336
  ,packetsForState = \case
    Playing -> Vector.concat
      [Vector.singleton (p Vector.! 0x00)
      ,Vector.slice 0x02 0x11 p
      ,Vector.singleton $ ambiguate $ Server.craftRecipeRequest $ Server.unhandled "Craft Recipe Request"
      ,Vector.drop 0x13 p
      ]
    x -> packetsForState protocol335 x
  }
  where
    p = packetsForState protocol335 Playing

-- | @'protcol335'@ is the same as @'protocol334'@.
protocol335 :: Configuration
protocol335 = protocol334
  {serverVersion = "Vanilla 1.12"
  ,protocolVersion = 335
  }

-- | @'protcol334'@ is the same as @'protocol333'@.
protocol334 :: Configuration
protocol334 = protocol333
  {serverVersion = "Vanilla 1.12-pre7"
  ,protocolVersion = 334
  }

-- | @'protcol333'@ is @'protocol332'@ but with updated packet types.
protocol333 :: Configuration
protocol333 = protocol332
  {serverVersion = "Vanilla 1.12-pre6"
  ,protocolVersion = 333
  ,packetsForState = \case
    Playing -> packetsForState protocol332 Playing Vector.//
      -- Unlock recipes now use VarInts instead of Shorts (Clientbound)
      -- Crafting Book Data now uses VarInts instead of Ints for actions
      [(0x17,ambiguate $ Server.craftingBookData @VarInt $ Server.unhandled "Crafting Book Data")
      ]
    x -> packetsForState protocol332 x
  }

-- | @'protocol332'@ is @'protocol331'@ with some packet id changes.
protocol332 :: Configuration
protocol332 = protocol331
  {serverVersion = "Vanilla 1.12-pre5"
  ,protocolVersion = 332
  ,packetsForState = \case
    Playing -> Vector.concat
      [Vector.slice 0x00 0x0d p
      ,Vector.singleton $ p Vector.! 0x10
      ,Vector.slice 0x0d 3 p
      ,Vector.slice 0x11 8 p
      ,Vector.singleton $ p Vector.! 0x20
      ,Vector.slice 0x19 7 p
      ]
    x -> packetsForState protocol331 x
  }
  where
    p = packetsForState protocol331 Playing

-- | @'protcol331'@ is the same as @'protocol330'@.
protocol331 :: Configuration
protocol331 = protocol330
  {serverVersion = "Vanilla 1.12-pre4"
  ,protocolVersion = 331
  }

-- | @'protocol330'@ is @'protocol329'@ with AdvancementTab and some extra changes.
protocol330 :: Configuration
protocol330 = protocol329
  {serverVersion = "Vanilla 1.12-pre3"
  ,protocolVersion = 330
  ,packetsForState = \case
    Playing -> Vector.snoc (packetsForState protocol329 Playing) (ambiguate $ Server.advancementTab $ Server.unhandled "Advancement Tab")
    x -> packetsForState protocol329 x
  }

-- | @'protcol329'@ is the same as @'protocol328'@.
protocol329 :: Configuration
protocol329 = protocol328
  {serverVersion = "Vanilla 1.12-pre2"
  ,protocolVersion = 329
  }

-- | @'protocol328'@ is @'protocol327'@ but with a new format for clientbound advancements
protocol328 :: Configuration
protocol328 = protocol327
  {serverVersion = "Vanilla 1.12-pre1"
  ,protocolVersion = 328
  }

-- | @'protocol327'@ is the same as @'protocol326'@
protocol327 :: Configuration
protocol327 = protocol326
  {serverVersion = "Vanilla 17w18b"
  ,protocolVersion = 327
  }

-- | @'protocol326'@ is @'protocol325'@ but with a new format for clientbound advancements, and it reverts 17w15a
protocol326 :: Configuration
protocol326 = protocol325
  {serverVersion = "Vanilla 17w18a"
  ,protocolVersion = 326
  }

-- | @'protocol325'@ is @'protocol324'@ but with a different array size type for @'PrepareCraftingGrid'@ and a new chat type "keybind"
protocol325 :: Configuration
protocol325 = protocol324
  {serverVersion = "Vanilla 17w17b"
  ,protocolVersion = 325
  ,packetsForState = \case
    Playing -> packetsForState protocol324 Playing Vector.//
      -- Prepare crafting grid uses shorts instead of @'Word8'@s
      [(0x01,ambiguate $ Server.prepareCraftingGrid @Short $ Server.unhandled "Prepare Crafting Grid")
      ]
    x -> packetsForState protocol324 x
  }

-- | @'protocol324'@ is @'protocol323'@ but with some clientbound changes
protocol324 :: Configuration
protocol324 = protocol323
  {serverVersion = "Vanilla 17w17a"
  ,protocolVersion = 324
  }

-- | @'protocol323'@ is the same as @'protocol322'@
protocol323 :: Configuration
protocol323 = protocol322
  {serverVersion = "Vanilla 17w16b"
  ,protocolVersion = 323
  }

-- | @'protocol322'@ is @'protocol321'@ but with the IllusionIllager and a few new noteblock sounds
protocol322 :: Configuration
protocol322 = protocol321
  {serverVersion = "Vanilla 17w16a"
  ,protocolVersion = 322
  }

-- | @'protocol321'@ is @'protocol320'@ but with the new "set bed color" id in Update Block Entity
protocol321 :: Configuration
protocol321 = protocol320
  {serverVersion = "Vanilla 17w15a"
  ,protocolVersion = 321
  }

-- | @'protocol320'@ is @'protocol319'@ but with the new @'PrepareCraftingGrid'@ packet.
protocol320 :: Configuration
protocol320 = protocol319
  {serverVersion = "Vanilla 17w14a"
  ,protocolVersion = 320
  ,packetsForState = \case
    Playing -> packetsForState protocol319 Playing Vector.//
      -- Use new instead of old
      [(0x01,ambiguate $ Server.prepareCraftingGrid @Word8 $ Server.unhandled "Prepare Crafting Grid")
      ]
    x -> packetsForState protocol319 x
  }

-- | @'protocol319'@ is @'protocol318'@ but without the Open Inventory Client Status action
protocol319 :: Configuration
protocol319 = protocol318
  {serverVersion = "Vanilla 17w13b"
  ,protocolVersion = 319
  }

-- | @'protocol318'@ is @'protocol317'@ but with a ton of new stuff
protocol318 :: Configuration
protocol318 = protocol317
  {serverVersion = "Vanilla 17w13a"
  ,protocolVersion = 318
  ,packetsForState = \case
    Playing -> Vector.concat
      [Vector.singleton $ p Vector.! 0x00
      ,Vector.singleton $ ambiguate $ Server.originalPrepareCraftingGrid $ Server.unhandled "Original Prepare Crafting Grid"
      ,Vector.slice 0x01 0x15 p
      ,Vector.singleton $ ambiguate $ Server.craftingBookData @Int32 $ Server.unhandled "Crafting Book Data"
      ,Vector.slice 0x16 8 p
      ]
    x -> packetsForState protocol317 x
  }
  where
    p = packetsForState protocol317 Playing
 
-- | @'protocol317'@ is @'protocol316'@ but with the MC|DebugNeighborsUpdate channel
protocol317 :: Configuration
protocol317 = protocol316
  {serverVersion = "Vanilla 1.11.1, 1.11.2"
  ,protocolVersion = 317
  }

-- | @'protocol316'@ is @'protocol315'@ but with new metadata stuff
protocol316 :: Configuration
protocol316 = protocol315
  {serverVersion = "Vanilla 16w50a"
  ,protocolVersion = 316
  }

-- | @'protocol315'@ is @'protocol314'@ but with new metadata stuff
protocol315 :: Configuration
protocol315 = baseProtocol
  {serverVersion = "Vanilla 1.11"
  ,protocolVersion = 315
  }

baseProtocol :: Configuration
baseProtocol = defaultConfiguration
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
      ,ambiguate $ Server.playerPosition $ handlePlayerPosition
      ,ambiguate $ Server.playerPositionAndLook $ handlePlayerPositionAndLook
      ,ambiguate $ Server.playerLook $ handlePlayerLook
      ,ambiguate $ Server.player $ handlePlayer
      ,ambiguate $ Server.vehicleMove $ Server.unhandled "Vehicle Move"
      ,ambiguate $ Server.steerBoat $ Server.unhandled "Steer Boat"
      ,ambiguate $ Server.playerAbilities $ handlePlayerAbilities
      ,ambiguate $ Server.playerDigging $ handlePlayerDigging
      ,ambiguate $ Server.entityAction $ handleEntityAction
      ,ambiguate $ Server.steerVehicle $ Server.unhandled "Steer Vehicle"
      ,ambiguate $ Server.resourcePackStatus $ Server.unhandled "Resource Pack Status"
      ,ambiguate $ Server.heldItemChange $ handleHeldItemChange
      ,ambiguate $ Server.creativeInventoryAction $ handleCreativeInventoryAction
      ,ambiguate $ Server.updateSign $ Server.unhandled "Update Sign"
      ,ambiguate $ Server.animation $ handleAnimation
      ,ambiguate $ Server.spectate $ Server.unhandled "Spectate"
      ,ambiguate $ Server.playerBlockPlacement $ handlePlayerBlockPlacement
      ,ambiguate $ Server.useItem $ handleUseItem
      ]
  ,serverVersion = "Base Protocol #315"
  ,protocolVersion = 315
  ,keepAliveThread = repeatedly 0 (keepAliveAction $ \i -> DescribedPacket (Client.keepAlive @VarInt 0x1F) (Client.KeepAlive i)) 20000000
  }
