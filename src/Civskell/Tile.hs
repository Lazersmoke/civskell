{-# LANGUAGE LambdaCase #-}
module Civskell.Tile where

import Control.Concurrent.STM

import Civskell.Data.Types
import Civskell.Data.Player
import Civskell.Tech.Network
import qualified Civskell.Window as Window
import qualified Civskell.Packet.Clientbound as Client

-- Bool isPolished
data Stone = Stone | Granite Bool | Diorite Bool | Andesite Bool
instance Block Stone where
  blockId = 1
  blockIdentifier = "minecraft:stone"
  blockMeta = \case
    Stone -> 0
    Granite False -> 1
    Granite True -> 2
    Diorite False -> 3
    Diorite True -> 4
    Andesite False -> 5
    Andesite True -> 6
  blockName = \case
    Stone -> "Stone"
    Granite False -> "Granite"
    Granite True -> "Polished Granite"
    Diorite False -> "Diorite"
    Diorite True -> "Polished Diorite"
    Andesite False -> "Andesite"
    Andesite True -> "Polished Andesite"

data Grass = Grass
instance Block Grass where
  blockId = 2
  blockIdentifier = "minecraft:grass"
  blockName Grass = "Grass Block"

data Dirt = Dirt | CoarseDirt | Podzol
instance Block Dirt where
  blockId = 3
  blockIdentifier = "minecraft:dirt"
  blockMeta = \case
    Dirt -> 0
    CoarseDirt -> 1
    Podzol -> 2
  blockName = \case
    Dirt -> "Dirt"
    CoarseDirt -> "Coarse Dirt"
    Podzol -> "Podzol"

data Chest = Chest CardinalDirection (TVar Inventory)
instance Block Chest where
  blockId = 0x36
  blockIdentifier = "minecraft:chest"
  blockMeta (Chest face _) = fromIntegral $ fromEnum face
  blockName _ = "Chest"
  onClick = Just $ \(Chest _facing items) xyz _ _ _ -> do
    sendPacket (Client.ChatMessage (jsonyText "How are you today? I'm doing just swimmingly myself ;)") 0)
    _wid <- openWindowWithItems (Window.Chest xyz) (jsonyText "Satisfied Chest") items
    return ()
    -- TODO: callback for changing items in chest
