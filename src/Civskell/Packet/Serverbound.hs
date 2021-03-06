{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | This module provides utilities for writing serverbound packets and @'InboundPacketDescriptor'@s to go with them,
-- as well as an extensive library of vanilla Minecraft packets and descriptors.
module Civskell.Packet.Serverbound 
  (
  -- * Utilties for making packets
  ServerboundPacketDescriptor,unhandled,defaultDescriptor
  -- * Vanilla serverbound packets
  -- ** Handshaking
  -- *** Handshake
  ,Handshake(..),handshake
  -- *** Legacy Server List Ping
  ,LegacyHandshake(..),legacyHandshake

  -- ** Playing
  -- *** Teleport Confirm
  ,TPConfirm(..),tpConfirm
  -- *** Prepare Crafting Grid
  ,PrepareCraftingGrid(..),prepareCraftingGrid
  -- *** Original Prepare Crafting Grid
  ,OriginalPrepareCraftingGrid(..),originalPrepareCraftingGrid
  -- *** Tab-Complete
  ,TabComplete(..),tabComplete
  -- *** Chat Message
  ,ChatMessage(..),chatMessage
  -- *** Client Status
  ,ClientStatus(..),clientStatus
  -- *** Client Settings
  ,ClientSettings(..),clientSettings
  -- *** Confirm Transaction
  ,ConfirmTransaction(..),confirmTransaction
  -- *** Enchant Item
  ,EnchantItem(..),enchantItem
  -- *** Click Window
  ,ClickWindow(..),clickWindow
  -- *** Close Window
  ,CloseWindow(..),closeWindow
  -- *** Plugin Message
  ,PluginMessage(..),pluginMessage
  -- *** Use Entity
  ,UseEntity(..),useEntity
  -- *** Keep Alive
  ,KeepAlive(..),keepAlive
  -- *** Player
  ,Player(..),player
  -- *** Player Position
  ,PlayerPosition(..),Civskell.Packet.Serverbound.playerPosition
  -- *** Player Position And Look
  ,PlayerPositionAndLook(..),playerPositionAndLook
  -- *** Player Look
  ,PlayerLook(..),playerLook
  -- *** Vehicle Move
  ,VehicleMove(..),vehicleMove
  -- *** Steer Boat
  ,SteerBoat(..),steerBoat
  -- *** Craft Recipe Request
  ,CraftRecipeRequest(..),craftRecipeRequest
  -- *** Player Abilities
  ,PlayerAbilities(..),playerAbilities
  -- *** Player Digging
  ,PlayerDigging(..),playerDigging
  -- *** Entity Action
  ,EntityAction(..),entityAction
  -- *** Steer Vehicle
  ,SteerVehicle(..),steerVehicle
  -- *** Crafting Book Data
  ,CraftingBookData(..),craftingBookData
  -- *** Resource Pack Status
  ,ResourcePackStatus(..),resourcePackStatus
  -- *** Advancement Tab
  ,AdvancementTab(..),advancementTab
  -- *** Held Item Change
  ,HeldItemChange(..),heldItemChange
  -- *** Creative Inventory Action
  ,CreativeInventoryAction(..),creativeInventoryAction
  -- *** Update Sign
  ,UpdateSign(..),updateSign
  -- *** Animation
  ,Animation(..),animation
  -- *** Spectate
  ,Spectate(..),spectate
  -- *** Player Block Placement
  ,PlayerBlockPlacement(..),playerBlockPlacement
  -- *** Use Item
  ,UseItem(..),useItem

  -- ** Status
  -- *** Request
  ,StatusRequest(..),statusRequest
  -- *** Ping
  ,StatusPing(..),statusPing

  -- ** Login
  -- *** Login Start
  ,LoginStart(..),loginStart
  -- *** Encryption Response
  ,EncryptionResponse(..),encryptionResponse

  -- * L
  ,image
  ) where

import Data.Int
import Data.Word
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bytes.Serial
import Data.Semigroup
import qualified Data.Text as T
import GHC.Generics
import qualified Data.ByteString as BS
import Crypto.Hash (hash,Digest,SHA1)
import Numeric (showHex)

import Civskell.Data.Types
import Civskell.Data.Logging

-- | A template for building an @'InboundPacketDescriptor'@ from an @'onPacket'@ function.
-- The reason we do this is to allow people to easily write handlers for these packets to put in
-- their @'supportedPackets'@ when they write custom servers. It's much more convienient than having
-- to rewrite the entire @'InboundPacketDescriptor'@ every time you want a custom handler. All vanilla
-- packets in this module are provided as @'ServerboundPacketDescriptor'@s
type ServerboundPacketDescriptor p = (p -> Civskell ()) -> InboundPacketDescriptor p

-- | A default packet handler that just prints an error message. For example:
--
-- @
--   'handshake' $ 'unhandled' "Packet name here"
-- @
--
-- is an @'InboundPacketDescriptor' 'Handshake'@.
unhandled :: T.Text -> p -> Civskell () 
unhandled t _ = lognyi $ "Unhandled Packet: " <> t

-- | A reasonable default for making @'ServerPacketDescriptor'@s. 
-- It uses @'ParThreading'@ for @'packetThreadingMode'@, and the @'Serial'@ instance's @'deserialize'@ for @'deserializePacket'@.
defaultDescriptor :: Serial p => ServerState -> T.Text -> (p -> [(T.Text,T.Text)]) -> ServerboundPacketDescriptor p
defaultDescriptor ss name pret handle = PacketDescriptor
  {packetState = ss 
  ,packetName = name 
  ,packetPretty = pret
  ,packetHandler = PacketHandler 
    {packetThreadingMode = ParThreading
    ,onPacket = handle
    ,deserializePacket = deserialize
    }
  }

---------------
-- Handshake --
---------------

-- | The vanilla Minecraft serverbound [Handshake](http://wiki.vg/Protocol#Handshake) packet.
data Handshake = Handshake VarInt ProtocolString Word16 VarInt deriving (Generic,Serial)
-- The protocol version. See [the wiki.vg page](http://wiki.vg/Protocol_version_numbers).
-- The server address the client is connecting to. Ex: @\"localhost"@
-- The port the client is connecting to.
-- The next state the client wants to go to. @1@ means @'Status'@, @2@ means @'LoggingIn'@.

-- | A reasonable @'ServerboundPacketDescriptor'@ for @'Handshake'@ packets.
handshake :: ServerboundPacketDescriptor Handshake
handshake = defaultDescriptor Handshaking "Handshake" $ \(Handshake protocol addr port newstate) ->
    [("Protocol Version",showText protocol)
    ,("Server Address",T.pack (unProtocolString addr) <> ":" <> showText port)
    ,("Requesting change to",case newstate of {1 -> "Status"; 2 -> "Login"; _ -> "Invalid"})
    ]  -- Normal handshake recieved


-- | The vanilla Minecraft serverbound [Legacy Server List Ping](http://wiki.vg/Protocol#Legacy_Server_List_Ping) packet.
data LegacyHandshake = LegacyHandshake Word8 LegacyString Int32
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'LegacyHandshake'@ packets.
legacyHandshake :: ServerboundPacketDescriptor LegacyHandshake
legacyHandshake = defaultDescriptor Handshaking "Legacy Handshake" $ \(LegacyHandshake _ _ _) -> []  -- Legacy response

instance Serial LegacyHandshake where
  serialize (LegacyHandshake proto hostname port) = putByteString legacyHandshakePingConstant *> serialize @Int16 (fromIntegral $ 7 + BS.length hostname) *> putWord8 proto *> serialize hostname *> serialize @Int32 port
  deserialize = do
    {-guard . (==legacyHandshakePingConstant) =<<-} 
    _ <- getByteString (BS.length legacyHandshakePingConstant) 
    len <- deserialize @Int16
    _ <- lookAhead (ensure $ fromIntegral len)
    LegacyHandshake <$> getWord8 <*> getByteString (fromIntegral $ len - 7) <*> deserialize @Int32

----------
-- Play --
----------

-- | The vanilla Minecraft serverbound [Teleport Confirm](http://wiki.vg/Protocol#Teleport_Confirm) packet.
data TPConfirm = TPConfirm VarInt deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'TPConfirm'@ packets.
tpConfirm :: ServerboundPacketDescriptor TPConfirm
tpConfirm = defaultDescriptor Playing "TP Confirm" $ \(TPConfirm i) -> [("Teleport Id",showText i)]

-- | The original version of @'PrepareCraftingGrid'@ as introduced in 17w13a
data OriginalPrepareCraftingGrid = OriginalPrepareCraftingGrid WindowId TransactionId Bool (ProtocolList Word8 (ProtocolString,Short,Word8,Word8)) deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'OriginalPrepareCraftingGrid'@ packets.
originalPrepareCraftingGrid :: ServerboundPacketDescriptor OriginalPrepareCraftingGrid
originalPrepareCraftingGrid = defaultDescriptor Playing "Original Prepare Crafting Grid" $ const []

-- | Legacy buggy type, but not the original
data PrepareCraftingGrid arraySize = PrepareCraftingGrid WindowId TransactionId Short (ProtocolList arraySize (WireSlot,Word8,Word8)) (ProtocolList arraySize (WireSlot,Word8,Word8)) deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'PrepareCraftingGrid'@ packets.
prepareCraftingGrid :: (Integral a,Serial a) => ServerboundPacketDescriptor (PrepareCraftingGrid a)
prepareCraftingGrid = defaultDescriptor Playing "Prepare Crafting Grid" $ const []

-- | The vanilla Minecraft serverbound [Craft Recipe Request](http://wiki.vg/Protocol#Craft_Recipe_Request) packet.
data CraftRecipeRequest = CraftRecipeRequest WindowId VarInt Bool deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'CraftRecipeRequest'@ packets.
craftRecipeRequest :: ServerboundPacketDescriptor CraftRecipeRequest
craftRecipeRequest = defaultDescriptor Playing "Craft Recipe Request" $ const []

-- | The vanilla Minecraft serverbound [Tab Complete](http://wiki.vg/Protocol#Tab-Complete_.28serverbound.29) packet.
data TabComplete = TabComplete ProtocolString Bool (ProtocolOptional BlockCoord) deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'TabComplete'@ packets.
tabComplete :: ServerboundPacketDescriptor TabComplete
tabComplete = defaultDescriptor Playing "Tab Complete" $ \(TabComplete textSoFar forceCommand (ProtocolOptional mBlockLookingAt)) -> [("Text so far",showText textSoFar),("Force command",showText forceCommand)] ++ (case mBlockLookingAt of {Just b -> [("Looking At",showText b)]; Nothing -> []})

-- | The vanilla Minecraft serverbound [Chat Message](http://wiki.vg/Protocol#Chat_Message_.28serverbound.29) packet.
data ChatMessage = ChatMessage ProtocolString deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'ChatMessage '@ packets.
chatMessage :: ServerboundPacketDescriptor ChatMessage 
chatMessage = defaultDescriptor Playing "Chat Message" $ \(ChatMessage msg) -> [("Message",T.pack $ show $ unProtocolString msg)]

-- | The vanilla Minecraft serverbound [Client Status](http://wiki.vg/Protocol#Client_Status) packet.
data ClientStatus = ClientStatus ClientStatusAction deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'ClientStatus'@ packets.
clientStatus :: ServerboundPacketDescriptor ClientStatus
clientStatus = defaultDescriptor Playing "Client Status" $ \(ClientStatus status) -> [("Status",showText status)]

-- TODO: type synonyms or newtypes or whatever
-- | The vanilla Minecraft serverbound [Client Settings](http://wiki.vg/Protocol#Client_Settings) packet.
data ClientSettings = ClientSettings ProtocolString Word8 VarInt Bool Word8 VarInt deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'ClientSettings'@ packets.
clientSettings :: ServerboundPacketDescriptor ClientSettings
clientSettings = defaultDescriptor Playing "Client Settings" $ \(ClientSettings (ProtocolString loc) viewDist chatMode chatColors skin hand) ->
    [("Locale",T.pack loc)
    ,("View Distance",showText viewDist)
    ,("Chat Mode",showText chatMode)
    ,("Chat colors enabled",showText chatColors)
    ,("Skin bitmask",showText skin)
    ,("Main Hand",showText hand)
    ]

-- | The vanilla Minecraft serverbound [Confirm Transaction](http://wiki.vg/Protocol#Confirm_Transaction_.28serverbound.29) packet.
data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'ConfirmTransaction'@ packets.
confirmTransaction :: ServerboundPacketDescriptor ConfirmTransaction
confirmTransaction = defaultDescriptor Playing "Confirm Transaction" $ \(ConfirmTransaction wid transId acc) -> [("Window Id",showText wid),("Transaction Id",showText transId),("Accepted",if acc then "Yes" else "No")]


-- | The vanilla Minecraft serverbound [Enchant Item](http://wiki.vg/Protocol#Enchant_Item) packet.
data EnchantItem = EnchantItem WindowId Word8 deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'EnchantItem'@ packets.
enchantItem :: ServerboundPacketDescriptor EnchantItem
enchantItem = defaultDescriptor Playing "Enchant Item" $ \(EnchantItem wid ench) -> [("Window Id",showText wid),("Enchantment Selected",showText ench)]

-- Slot Number
-- THIS SERIAL INSTANCE IS INCORRECT:
-- Button is in InventoryClickMode but should be before the transaction Id
-- | The vanilla Minecraft serverbound [Click Window](http://wiki.vg/Protocol#Click_Window) packet.
data ClickWindow = ClickWindow WindowId Short TransactionId InventoryClickMode WireSlot
instance Serial ClickWindow where
  serialize = error "Unimplemented: serialize @Server.Click Window"
  deserialize = error "Unimplemented: deserialize @Server.Click Window"
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'ClickWindow'@ packets.
clickWindow :: ServerboundPacketDescriptor ClickWindow
clickWindow = defaultDescriptor Playing "Click Window" $ \(ClickWindow wid slotNum transId invMode item) -> [("Window Id",showText wid),("Slot Number",showText slotNum),("Transaction Id",showText transId),("Inventory Mode",showText invMode),("Subject Item",showText item)]

 {- This code kept as reference for when we write `instance Serial ClickWindow`
   -
   - parsePacket = do
    specificVarInt 0x07 <?> "Packet Id 0x07"
    wid <- parseWID
    slotNum <- parseShort
    b <- anyWord8
    transId <- parseShort
    mode <- choice
      [guard (elem b [0,1]) *> specificVarInt 0x00 *> pure (NormalClick (b == 1))
      ,guard (elem b [0,1]) *> specificVarInt 0x01 *> pure (ShiftClick (b == 1))
      ,guard (elem b [0..8]) *> specificVarInt 0x02 *> pure (NumberKey b)
      ,guard (b == 2) *> specificVarInt 0x03 *> pure MiddleClick
      ,guard (elem b [0,1]) *> specificVarInt 0x04 *> pure (ItemDropOut (b == 1))
      ,guard (elem b [0,1,2,4,5,6,8,9,10]) *> specificVarInt 0x05 *> pure (PaintingMode b)
      ,guard (b == 0) *> specificVarInt 0x06 *> pure DoubleClick
      ]
    sl <- Item.parseSlot
    pure $ ClickWindow wid slotNum transId mode sl
  -}

-- | The vanilla Minecraft serverbound [Close Window](http://wiki.vg/Protocol#Close_Window_.28serverbound.29) packet.
data CloseWindow = CloseWindow WindowId deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'CloseWindow'@ packets.
closeWindow :: ServerboundPacketDescriptor CloseWindow
closeWindow = defaultDescriptor Playing "Close Window" $ \(CloseWindow wid) -> [("Window Id", showText wid)]

-- | The vanilla Minecraft serverbound [Plugin Message](http://wiki.vg/Protocol#Plugin_Message_.28serverbound.29) packet.
data PluginMessage = PluginMessage ProtocolString BS.ByteString
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'PluginMessage'@ packets.
pluginMessage :: ServerboundPacketDescriptor PluginMessage
pluginMessage = defaultDescriptor Playing "Plugin Message" $ \case
    (PluginMessage "M C|Brand" cliBrand) -> [("Client Brand",showText (BS.tail cliBrand))]
    (PluginMessage chan bs) -> [("Channel",showText chan),("Payload",showText bs)]
instance Serial PluginMessage where
  serialize (PluginMessage ch dat) = serialize ch *> putByteString dat
  deserialize = PluginMessage <$> deserialize @ProtocolString <*> (getByteString . fromIntegral =<< remaining)

-- | The vanilla Minecraft serverbound [Use Entity](http://wiki.vg/Protocol#Use_Entity) packet.
data UseEntity = UseEntity EntityId EntityInteraction deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'UseEntity'@ packets.
useEntity :: ServerboundPacketDescriptor UseEntity
useEntity = defaultDescriptor Playing "Use Entity" $ \(UseEntity targetEID action) -> [("Target",showText targetEID),("Action",showText action)]

-- | The vanilla Minecraft serverbound [Keep Alive](http://wiki.vg/Protocol#Keep_Alive_.28serverbound.29) packet.
data KeepAlive a = KeepAlive a deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'KeepAlive'@ packets.
keepAlive :: (Serial a,Show a) => ServerboundPacketDescriptor (KeepAlive a)
keepAlive = defaultDescriptor Playing "Keep Alive" $ \(KeepAlive i) -> [("Keep Alive Id",showText i)]

-- | The vanilla Minecraft serverbound [Player](http://wiki.vg/Protocol#Player) packet.
data Player = Player Bool deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'Player'@ packets.
player :: ServerboundPacketDescriptor Player
player = defaultDescriptor Playing "Player" $ \(Player grounded) -> [("On Ground",showText grounded)]

-- | The vanilla Minecraft serverbound [Player Position](http://wiki.vg/Protocol#Player_Position) packet.
data PlayerPosition = PlayerPosition (Double,Double,Double) Bool deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'PlayerPosition'@ packets.
playerPosition :: ServerboundPacketDescriptor PlayerPosition
playerPosition = defaultDescriptor Playing "Player Position" $ \(PlayerPosition (x,y,z) grounded) -> [("Positon",showText (x,y,z)),("On Ground",showText grounded)]

-- | The vanilla Minecraft serverbound [Player Position And Look](http://wiki.vg/Protocol#Player_Position_And_Look_.28serverbound.29) packet.
data PlayerPositionAndLook = PlayerPositionAndLook (Double,Double,Double) (Float,Float) Bool deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'PlayerPositionAndLook'@ packets.
playerPositionAndLook :: ServerboundPacketDescriptor PlayerPositionAndLook
playerPositionAndLook = defaultDescriptor Playing "Player Position And Look" $ \(PlayerPositionAndLook (x,y,z) (yaw,pitch) grounded) -> [("Positon",showText (x,y,z)),("Looking",showText (yaw,pitch)),("On Ground",showText grounded)]

-- | The vanilla Minecraft serverbound [Player Look](http://wiki.vg/Protocol#Player_Look) packet.
data PlayerLook = PlayerLook (Float,Float) Bool deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'PlayerLook'@ packets.
playerLook :: ServerboundPacketDescriptor PlayerLook
playerLook = defaultDescriptor Playing "Player Look" $ \(PlayerLook  (yaw,pitch) grounded) -> [("Looking",showText (yaw,pitch)),("On Ground",showText grounded)]

-- | The vanilla Minecraft serverbound [Vehicle Move](http://wiki.vg/Protocol#Vehicle_Move_.28serverbound.29) packet.
data VehicleMove = VehicleMove (Double,Double,Double) (Float,Float) deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'VehicleMove'@ packets.
vehicleMove :: ServerboundPacketDescriptor VehicleMove
vehicleMove = defaultDescriptor Playing "Vehicle Move" $ \(VehicleMove _ _) -> []

-- | The vanilla Minecraft serverbound [Steer Boat](http://wiki.vg/Protocol#Steer_Boat) packet.
data SteerBoat = SteerBoat Bool Bool deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'SteerBoat'@ packets.
steerBoat :: ServerboundPacketDescriptor SteerBoat
steerBoat = defaultDescriptor Playing "Steer Boat" $ \(SteerBoat _ _) -> []

-- | The vanilla Minecraft serverbound [Player Abilities](http://wiki.vg/Protocol#Player_Abilities_.28serverbound.29) packet.
data PlayerAbilities = PlayerAbilities AbilityFlags Float Float deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'PlayerAbilities'@ packets.
playerAbilities :: ServerboundPacketDescriptor PlayerAbilities
playerAbilities = defaultDescriptor Playing "Player Abilities" $ \(PlayerAbilities (AbilityFlags i f af c) flySpeed fovMod) -> u i "Invulnerable" <> u f "Flying" <> u af "Allow Flying" <> u c "Creative" <> [("Flying Speed",showText flySpeed),("F OV Modifier",showText fovMod)]
  where
    u b s = if b then [(s,"")] else []

-- | The vanilla Minecraft serverbound [Player Digging](http://wiki.vg/Protocol#Player_Digging) packet.
data PlayerDigging = PlayerDigging PlayerDigAction deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'PlayerDigging'@ packets.
playerDigging :: ServerboundPacketDescriptor PlayerDigging
playerDigging = defaultDescriptor Playing "Player Digging" $ \(PlayerDigging action) -> case action of
    StartDig bc side -> [("Action","Start Digging"),("Block",showText bc),("Side",showText side)]
    StopDig bc side -> [("Action","Stop Digging"),("Block",showText bc),("Side",showText side)]
    EndDig bc side -> [("Action","Finished Digging"),("Block",showText bc),("Side",showText side)]
    DropItem isStack -> [("Action","Drop " <> if isStack then "Stack" else "Item")]
    ShootArrowOrFinishEating -> [("Action","inb4 Minecraft")]
    SwapHands -> [("Action","Swap items in hands")]

-- | The vanilla Minecraft serverbound [Entity Action](http://wiki.vg/Protocol#Entity_Action) packet.
data EntityAction = EntityAction PlayerId PlayerEntityAction deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'EntityAction'@ packets.
entityAction :: ServerboundPacketDescriptor EntityAction
entityAction = defaultDescriptor Playing "Entity Action" $ \(EntityAction eid fire) -> (("Entity Id",showText eid):) $ let u b = if b then id else ("Stop "<>) in case fire of
    Sneak b -> [("Action",u b "Sneak")]
    Sprint b -> [("Action",u b "Sprint")]
    HorseJumpStart s -> [("Action","Horse Jump"),("Horse Jump Strength",showText s)]
    HorseJumpStop -> [("Action","Stop Horse Jump")]
    LeaveBed -> [("Action","Leave Bed")]
    HorseInventory -> [("Action","Open Horse Inventory")]
    ElytraFly -> [("Action","Elytra Fly")]

-- | The vanilla Minecraft serverbound [Steer Vehicle](http://wiki.vg/Protocol#Steer_Vehicle) packet.
data SteerVehicle = SteerVehicle Float Float Word8 deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'SteerVehicle'@ packets.
steerVehicle :: ServerboundPacketDescriptor SteerVehicle
steerVehicle = defaultDescriptor Playing "Steer Vehicle" $ \(SteerVehicle _ _ _) -> []

-- | The vanilla Minecraft serverbound [Crafting Book Data](http://wiki.vg/Protocol#Crafting_Book_Data) packet.
data CraftingBookData a = CraftingBookDisplayedRecipe Int32 | CraftingBookStatus Bool Bool
instance (Num a,Serial a,Show a,Eq a) => Serial (CraftingBookData a) where
  serialize (CraftingBookDisplayedRecipe rId) = serialize @a 0 *> serialize rId
  serialize (CraftingBookStatus isOpen hasFilter) = serialize @a 1 *> serialize isOpen *> serialize hasFilter
  deserialize = deserialize @a >>= \case
    0 -> CraftingBookDisplayedRecipe <$> deserialize @Int32
    1 -> CraftingBookStatus <$> deserialize @Bool <*> deserialize @Bool
    x -> error $ "deserialize @CraftingBookData: Bad 'Type' indicator " <> show x
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'CraftingBookData'@ packets.
craftingBookData :: (Num a,Serial a,Show a,Eq a) => ServerboundPacketDescriptor (CraftingBookData a)
craftingBookData = defaultDescriptor Playing "Crafting Book Data" $ \case 
  (CraftingBookDisplayedRecipe i) -> [("Recipe ID",showText i)]
  (CraftingBookStatus bookOn filterOn) -> [("Book",if bookOn then "Active" else "Inactive"),("Filter",if filterOn then "Active" else "Inactive")]

-- | The vanilla Minecraft serverbound [Resource Pack Status](http://wiki.vg/Protocol#Resource_Pack_Status) packet.
data ResourcePackStatus = ResourcePackStatus VarInt deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'ResourcePackStatus'@ packets.
resourcePackStatus :: ServerboundPacketDescriptor ResourcePackStatus
resourcePackStatus = defaultDescriptor Playing "Resource Pack Status" $ \(ResourcePackStatus _) -> []

-- Nothing means tab closed, Just a means tab a was opened
-- | The vanilla Minecraft serverbound [Advancement Tab](http://wiki.vg/Protocol#Advancement_Tab) packet.
data AdvancementTab = AdvancementTab (Maybe ProtocolString)
instance Serial AdvancementTab where
  serialize (AdvancementTab (Just tab)) = serialize @VarInt 0 *> serialize tab
  serialize (AdvancementTab Nothing) = serialize @VarInt 1
  deserialize = deserialize @VarInt >>= \case
    0 -> AdvancementTab . Just <$> deserialize @ProtocolString
    1 -> pure (AdvancementTab Nothing)
    x -> error $ "deserialize @AdvancementTab: Bad 'Action' indicator " <> show x
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'AdvancementTab'@ packets.
advancementTab :: ServerboundPacketDescriptor AdvancementTab
advancementTab = defaultDescriptor Playing "Advancement Tab" $ \_ -> []

-- | The vanilla Minecraft serverbound [Held Item Change](http://wiki.vg/Protocol#Held_Item_Change_.28serverbound.29) packet.
data HeldItemChange = HeldItemChange Short deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'HeldItemChange'@ packets.
heldItemChange :: ServerboundPacketDescriptor HeldItemChange
heldItemChange = defaultDescriptor Playing "Held Item Change" $ \(HeldItemChange i) -> [("Slot",showText i)]

-- | The vanilla Minecraft serverbound [Creative Inventory Action](http://wiki.vg/Protocol#Creative_Inventory_Action) packet.
data CreativeInventoryAction = CreativeInventoryAction Short WireSlot deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'CreativeInventoryAction'@ packets.
creativeInventoryAction :: ServerboundPacketDescriptor CreativeInventoryAction
creativeInventoryAction = defaultDescriptor Playing "Creative Inventory Action" $ \(CreativeInventoryAction slotNum item) -> [("Slot",showText slotNum),("New Item", showText item)]

-- | The vanilla Minecraft serverbound [Update Sign](http://wiki.vg/Protocol#Update_Sign) packet.
data UpdateSign = UpdateSign BlockCoord (ProtocolString,ProtocolString,ProtocolString,ProtocolString) deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'UpdateSign'@ packets.
updateSign :: ServerboundPacketDescriptor UpdateSign
updateSign = defaultDescriptor Playing "Update Sign" $ \(UpdateSign _ _) -> []

-- | The vanilla Minecraft serverbound [Animation](http://wiki.vg/Protocol#Animation_.28serverbound.29) packet.
data Animation = Animation Hand deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'Animation'@ packets.
animation :: ServerboundPacketDescriptor Animation
animation = defaultDescriptor Playing "Animation" $ \(Animation hand) -> [("Hand",showText hand)]

-- | The vanilla Minecraft serverbound [Spectate](http://wiki.vg/Protocol#Spectate) packet.
data Spectate = Spectate UUID deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'Spectate'@ packets.
spectate :: ServerboundPacketDescriptor Spectate
spectate = defaultDescriptor Playing "Spectate" $ \(Spectate uuid) -> [("U UI D",showText uuid)]

-- | The vanilla Minecraft serverbound [Player Block Placement](http://wiki.vg/Protocol#Player_Block_Placement) packet.
data PlayerBlockPlacement = PlayerBlockPlacement BlockCoord BlockFace Hand (Float,Float,Float) deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'PlayerBlockPlacement'@ packets.
playerBlockPlacement :: ServerboundPacketDescriptor PlayerBlockPlacement
playerBlockPlacement = defaultDescriptor Playing "Player Block Placement" $ \(PlayerBlockPlacement block side hand _cursorCoord) -> [("Block",showText block),("Side",showText side),("Hand",showText hand)]

-- | The vanilla Minecraft serverbound [Use Item](http://wiki.vg/Protocol#Use_Item) packet.
data UseItem = UseItem Hand deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'UseItem'@ packets.
useItem :: ServerboundPacketDescriptor UseItem
useItem = defaultDescriptor Playing "Use Item" $ \(UseItem hand) -> [("Hand",showText hand)]

-----------
-- Login --
-----------

-- | The vanilla Minecraft serverbound [Login Start](http://wiki.vg/Protocol#Login_Start) packet.
data LoginStart = LoginStart ProtocolString deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'LoginStart'@ packets.
loginStart :: ServerboundPacketDescriptor LoginStart
loginStart = defaultDescriptor LoggingIn "Login Start" $ \(LoginStart name) -> [("Username",T.pack $ unProtocolString name)]

-- | The vanilla Minecraft serverbound [Encryption Response](http://wiki.vg/Protocol#Encryption_Response) packet.
data EncryptionResponse = EncryptionResponse BS.ByteString BS.ByteString 
instance Serial EncryptionResponse where
  serialize (EncryptionResponse ss vt) = do
    serialize @VarInt (fromIntegral $ BS.length ss) 
    putByteString ss
    serialize @VarInt (fromIntegral $ BS.length vt) 
    putByteString vt
  deserialize = EncryptionResponse <$> (deserialize @VarInt >>= getByteString . fromIntegral) <*> (deserialize @VarInt >>= getByteString . fromIntegral)

-- | A reasonable @'ServerboundPacketDescriptor'@ for @'EncryptionResponse'@ packets.
encryptionResponse :: ServerboundPacketDescriptor EncryptionResponse
encryptionResponse = defaultDescriptor LoggingIn "Encryption Response" $ \(EncryptionResponse ss vt) ->
    [("Shared Secret Hash",(T.take 7 $ showText (hash ss :: Digest SHA1)) <> "...")
    ,("Verify Token Hash",(T.take 7 $ showText (hash vt :: Digest SHA1)) <> "...")
    ]

------------
-- Status --
------------

-- | The vanilla Minecraft serverbound status [Request](http://wiki.vg/Protocol#Request) packet.
data StatusRequest = StatusRequest deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'StatusRequest'@ packets.
statusRequest :: ServerboundPacketDescriptor StatusRequest
statusRequest = defaultDescriptor Status "Status Request" $ \StatusRequest -> []

-- | The vanilla Minecraft serverbound status [Ping](http://wiki.vg/Protocol#Ping) packet.
data StatusPing = StatusPing Int64 deriving (Generic,Serial)
-- | A reasonable @'ServerboundPacketDescriptor'@ for @'StatusPing'@ packets.
statusPing :: ServerboundPacketDescriptor StatusPing
statusPing = defaultDescriptor Status "Status Ping" $ \(StatusPing i) -> [("Ping Token",T.pack $ "0x" <> showHex i "")]

-- TODO: make this not cancer
-- | The server list image for Civskell. Yes, it's stored as a @'String'@. Yes, it's in this module for no reason. Yes, I'm too lazy to change it.
image :: String
image = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAIAAAAlC+aJAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsSAAALEgHS3X78AAAc1klEQVRo3pV6V6yt13HezKzy191Ovf3y3ktKIiVS1SItR1GzrQSy8xAbTuIYiVwDJw/OS4AUBHky4rylGEbgIEgeggSOAzsucIubZHVRokVSIimJ5dZzzj1tt7+sNpOHTR1dXZ7rxOth49//3pg137Q1a2bwYx/72O3bt2ezmbVWKeW9FxEiQkQAEBG4b6ECSQAsoBMSiGjxFmJCYIQUJYlktiCiEBIRYUioiIEYBAA0oRJGllZnir2GpAhjgi4io9VZruL8vn1XzyKSZZn3HhGNMW3blmX5yCOPqLIs27YFACJiZhFRSimlROQU7gGkn5HJ0RTALk9NTimpYgpVZBXE2rwiZV3vUmSrtbWaa50UASlBJSACkVUQHSDpBOQTeUEiVRjMwZGbsTJw2kJEREwpMTMRpZS8933fq7IsAcAYg4gxRiJaIYEHLBxsQ/Dol0Q22FEr1qR+TebW5OI7lGS0ZgZBxWRmrXdLHXoPySn0ihhQR66jjGpYKGPRloggsYPgmIhNBXL61imllVGsZL1isus6fNOb3rTCx8zMrJQiIu+9UupUQuw7ympRWYwRUzCaBCAk0ADB95JcrskazDM9qgdKY2UhBvEdLLu46KURaUE8QJ4NrJ/asEBjej0MqHL2JfVtOl0DMcaVaayES0Srl3ol+BWGlamtrOhUKiJSGONjJzFYBUQQQvSoRZdn4fbDD29cGFRXJ3SpjrXMN/OjtUG2N1c6o6h46sPuEm4u1M0D2pvHZ3duO8p9sZZAZZhK6IRd6yOY0wEYY1YWvmJ4JWtExIcffvhE8CmlFfcrhdzH+uqhB2UJjEIfIfp2Q/t3bsvj29mjZ/LxaDA/PkLE4WTcu9C4kBiaEKzSGZBKyRJkGZAKLGGnrb4xs5+6lZ6/Nfch1XUNOms8F+BOBbAKMCeyf90cmNXGxgYinjC98pVT3Xe1tPhAto1YQff+i/h3nqg/eJG2ceqFIIXl9LDSeHZ9NKkKw7GdHg6SUr6Pvu1TmLm4N097C3N3UdW6fXgcn9yW8yPdQHm71d7LSIVVsHrQWjG2EvfqK775zW9eqWb1mzEGAO7zgXvxtKoY9Dvv3owffnT7wtjMj49ckHI02sripK696zQpY0wfuPfp9t4B56ZzzieldBY49W0jkjKrURdd1+UKN+rcJ/zqbvjMrnq5G1oVT/c9ZmOMiIQQTgweANT6+vrqy0r2K1dGROIgpgg6L+Oc3ULnFSLdSpOtXP7uY/zDb8HzpRzePqBsOFwb1HZ2ti7KMluvtVse77ZwPU12p92QegsytReo3rqc92fgGFVO+Qijb4MMlC6Gg6NIbR8vrJdbIwP94qWurqBTJkvRK4i9KhEJ/JK0XUl5FYJOGH49Cr3x4LDWzmazyaA8CmZDN11S+6n42PD6z330SteHNL1bbF16ZjdeHMMTa6kJsJ7j03fp6Z343Kv7ex3tpXqZdKWVUmqWtEHZNs1Qx61R8eRF8+51jz4G5Q6O/TIUQbWmbQCHu3o0n7X/8xXNrlN57ckM4rTHHHQGMZxqTq8DOOXQRYMQU7/MqsGBDGS+92OP8T/6a29Dbnae//Jg+y3PHPiRnr5jrT/qyk/cqP501780Mwepjt1ykokSrwE1cEThmFBRVMU02TbIuk2Xh/zWLfUDF+y7rpV3F9M0bYtcvda43XlV8rTX419+xu+0bBVFgSzMe7uuIZ3qEt8GcB8Gn3SZEfo5aftaGPzQVffvPohH2flmfuia5cHxclJAVZs/vjv81edhf7+L6KJAlmUcvM5yFwTIJI6ZAkzeaCUro43BhQCoeyx1pr5va/EP3zMaD+nG3kxALYRi784PaIcn/+bPprcaWxpqGWtpHevTo9P6+vp9x/XrhzaoQsXo3YKGP3hx8Y/fRSEfpW4GARpd5sZHXfz7p+k/P6OmPWgKqdzIlFYSUkoCmKLXipTJDDgGiKqceWq9ZFlG2oAyXrTh5Uvt2m+82GNq3/XIBQKixc28rJcBLhb949fO/dkLOwvIJYQQ04OOpu8A8B0Hh6LUT3s9vFa1/+IpDcu9QxltDW3TSlLy2Z34C39y+NJ8vA5hKMdiwLUtQZy3vqcSkDj0LoQWi15M4yDLTY6cUkyUcWQTu7Wi5TS0MutRPnGjOlz2j237ftan5APLFKrLevrElc0/eeGwGgyZMpJ0empzrwndF7gw9qPS/rMPTcrDFyYX3lTExf4ylLL8Urv9y8/BUSeTbtdn47Y6L91BRfHJK+tFmkUgZq7IY4pUDtpoP/31/R7tWEXvfW/XnahKM3RHvrCUirW0TCZ+rVl/fGL/7V8XC57NwHI7PZ5fPrf161/e//nPhkmdCdCp2fEDNSDGhhB/+j3VpSLYtYs69VmWDavs9/ezX/zUUcObJjkab8xiLNK8liCh/6mn1v/2Q4vv2fZvXsP3P7L2vovqA5fMUxeKL99uXmhGQVcakwhwihmGngDtmWXkFrXSo4lMkz/85E791EWTjm6UEDTh7iJevXRhufvKc+5sBt8R/r8N4MLGJIJi0gwEIrXhvl1AOfa9e/Jy/RNviT62Y+OHGTrQn7yNv/T5QDrX6IiEY7CIABRRgR784IWdGcAub+tmahQLMqagM9t07lO3/FaR9rjOoB9AewijDAFjn0E0CCmFRDpgvnThmX3144+pecqcmMNoJ27n7RdHv3uDDqM6r5vjVIpARe0SslwBMlMUYWYSUCBaYWDM62F0jlB+5nGIAFk9jpgNBoNb0/i/n9l90CF/6HXKRjb16ei6KYeNHvl8va0uJdDf/VC9XdGRg0GcWpsLkYHwIDqv7S/+9Wc9FkMn+swg2+8xl8XfeqsdQbNMWhMgh4RaofjEhEIJNKK8rhpJXRJWZeyW3/um4RPVcRIAFiJqJf+V55qvH9ODNj6njhsvthhOCqtABrIczV7J2oMuxLdt66cu5Q3bkXLe+wCm0HBq7AYAIP1bO4PPvzZPwS2axutq1sv3X0pXKzdLmSZU7BIYIvIJFYBa2zijQAARgZWkgJlLccOGf/mhSdv3gmQkrI+qT77W/oc/x7VCJ8FTAXiqPn+r+61X9R+9Sp96rbl05eyZMk5hDdycQ8eMf3QDR4adC5EyolPyxVX4zpCj6Js7+1fK7rDxGfJdRzU0HvNnDkxBqMEFygglic6kp8hCwCJIRBpZ50V0/t2XJ4/Vy1lxwUceViai+fXn5zovAz8wVSzcwdzLIY/vpuGX7/inv7kjvvV3X42u+eqef/uwuzqIB97WhU0qiwkfRMcnHsn8FT/+7L6dDIfiZkVRzLvw7gvVuo0p9qgsMxOwNsSChIiEnEAESFIUQUrdO87oo2XnXd+4kDD/zKuLL+7AeThsOX/QxlSMhgb07HbF3WDrwmdv9rsySaJSStGOLq/bj1zN5m0PpFESKDrVhBAR8rUgStfjzy3PrA/s9sboch1HVXF+QG+dRI4+oEUJwtGgBDSkEIgTCzASM/u+m5T6ctEd9NLP9iKnaZ9+8/ljLyr0fWnVA524Z/G+1mglGPCv3T26NY/WZL3orVp5MB+8gFu1Wjih5CzwfRWHbx+gyA4L62b7M/fcjtsYjYiosmDZvfecVkp1Yi0KMGN0iSwhMHMUQUAiFGbeHNebNoQQTD4orQ4CLxzBwMIsP1/6wwcBMFkRVB7rs9OgxC2euHJmY5iVKvWSVf7o1d3peds8dn7cMhqIErtT6w4AoJoDY0xNrgwHX9zXGnmRbSuC4LvHtmxmrBellRAwRC/KUkyJVVYqTs5hNmJTX+LbC5+OQ3Zw3G+O1B/sUtMNM85G/pV5tnZfMniyKPYSE3KDOQTMfviyLeZ3b7qI/fLG0jeAs6A+MryTFDqdRxzgd64Tan02jMl1kJU2+/Ru/o1muM0HndQdWaPlvJ0BJ46m1xuVYg5LUtYwEiAjSt+3rj2+sDlSEnrXsfg7B+03by6WvgWrUNXexTfKbLUqClCvRdcsnfzAhfZtV7aVKSxBS6OIpgtpvuwvnL90LnNaa+0XD6o7EYFGWl1rQ3A7R8vOhelstncwmy7bIjeGBCVxDFFEa00+ht6FGL02BChDw+997PL57Y21UZ3ldOuuv3mosDIRQ5T8QSkhACCH5P0CB5fz5U9816hMS2NMXZgmivOxTzRv+2GVf9dWaF1viU/A34sEEQkYEQEVoAohvHbYJJZl18+7sOzDpMqIgwJWEhMoACIASAjMLImdc7WK2xUYjRe3J2c3apfyaZdlhWJxienE+d64GjYWXJvwp99TXxprSD6EoIHzuFSxF9SAys32vu8yJtfGfHyfAk+8mWMQEQECpVB4dwl5XmplTVEH0ONSSeiIwCpIoFJKpBC0zQgkCfsEk9IYPxcRC35rYG1eTDsnyWlMRCS+OTHZ+1VvbMP6Q2f7D10ru6SBFBWld82ZSk+qzCioC6vc7J3n8mub+VHKT71eiYhwFFmVf1AjH0ZdFEVRZkqZmKTWApIQkUCYMhImCV4YUSBFQZOPB2UGQWdVdD13DikyhEy0eAZka+g+6z95sBh7F3/82tzYLAgue6fyQVkNFjSiYlJRKg2MR4Mi0x99dOJdd2I590lEIRBRAgEAJXEejQbONYZuGUKwmEirgCqllEAZQiIUEEGWGDmKdl1LnBYuMdi+i5SJKsRQxUEHTPiAstmqyvfRy+naCACg65pMq9Qvesx/7WV5YUZDwyb1lA1uLeCvngmX8+WqlnPfTRAACBDg9QKpkujApthbYUtCKAoSaRuBIguT0ZiI4HWFCZIyOTAPqgJUwaiUKYs6S5RiIK1r0KZ1/kEAkqk+/miCtUvz3deKorDW1iq8dOvgV59d/Pmtps4Iop95uTmX87j/2ERW9Zt7Maw+V2+SMIAgCJPGFBXxoMoLm2HygErQAKIgSYzkOTNKYuq1SiPNey7b9wa563pQ1WA7N2smA2oULql3QjmgWalSEcTQRTQTPj7AtR95JKAa+umOHlxsOFC/SED/8avDs2X6P3fW9+VsVpel9qXC/Tj6q1vT46jXcqsgi9IqGTpUiUweFmCyJqARTFBYwor8XQdJWKd0pkyHbCS2KUkJorjvNJLRxDExkCITOaWU+iCaGBXllEZFNsS+hTLGRMhkNEjKsqyFXLp5MdpW3O/w+Kl67/svkchSHHqZz/YWR2h+4TNyZ9qJMuLmX9oNa8MBRK/8wom6cunsezeb3SUtJeSmNnw47I40NIts24e+zCjXRERd5E3VmazIjJ2MB1sDy7YKSQykpC0zG1KkIaUUgBQZHYW89ztzR5yYmWJbWbhQuBmUCUmLFwHikMgqQlaZc70o60F+6p32bZtgNbSq6pezUZF//iD/w9ulzSrQGfnln77q2RSTHNcKFRIMC/3Euiwc1FlisUtdiJlApsE5IlTccWiiCKv8kbpn0SnJoMo2B3oebGIy4FkVEIMCJE5BKQXKkgBp471/YWcZODFzco1Bfvu5IjBom6tvdRmCaJtaLtYgtPMA339Ff8/VulXZ2trWUoGO2TGmX3s2apkPiFNK1ugXj+G5vTCxqSgKoyD55j2X63MTmxs66p2PEv3iuIEqAzQmehcEImBRFNeGYd4Fz5RS0ApvzSKQUSlEVCiMiSmlZK1FpVP0SqnE8NW94FgjCjMz0NsfWh/KDEgBABGyspn4lBJJJG22bfcPnpyE4I48+JhMf4S2/vXnFt84iAPoxDcpBbSlS/iHLy0lsRczyqHM7MNDeUjdXNy+dWXS/eQTw595yoyyrIGMRQmiKQfMcS0L2xX2PgbKvaQDb24d9FmWJUkCbBSyKA2oUGmUmEJDplJKfXNOM4cj5GXg6NNGpd9Wzl5ZZixiFCbWmTvw5bZtd449ffx9k7eupVkYjCF94/bO+Sz/isPfe2VQDgBxq+sPiSAIEYdn9/kI102hSnQhcS/lT36gPjxszl0bPxwSI/3215cv9wUbMEpH0Oymj5+N47qYOu48DOrsuX0171JWUvAKYqcVBtaEJo+BAcBISsGTtnc6e+OoTd7tz/3to6ZbzD98GQEokgVOlJwgAamA5u2b/MOP133fQ1ag0pbwUG/8zkt9Q5WGFIQcKCISEQ182OPnDvNaIwBxctPOX63lHeuL9fbOfL6LaJ48D9IvNUcFuGzbSZY+ctXawo6zxBxdxE+80jNrYg7KKnAkwqAIUHU+IKJWAMxE+jjQnYN5TP5g2d2dtrPZ9LsfGuZ5rmwBHK10UG6m5eGcRh9/3+WzWfCY3d0//MrLt3Mqfv+W+6OvdeOi7Vtlwm6e55HFajCanHN//NI0Nsed98hpAsfLuV/Y8taymmZDSfaph2sqN5GDQUSRS5vD916qY4yVSWWZL3v35RtzIg0xsLJaAksMAoQQq0wDS081pz5yuFjT/3h1bYTLy2tDqug2bm8p96PXFm4JQrkm1fvQCX106+hKsbh+0Ly0M7+7u0fzg5uw9YdfuFXUQx+LwqYEWUpBk3KpNnCosuLpXfyKW5vEvifrsRqa4wnwmaEZSjesUmFH14rdTo060SVOf+JRqCaTUVGZcnBts/7c3aGf3c1V49Uq/aFejNKiNjY27suoUkohhPW1zUc2uFs0w5yOu/iOC1tfvDPb8UXujo+qhx4dp3/6PdUWLZds87yMQn587Tc/9/IrC0PVZtc2ObHSFJlT71jFTOfJ2M7LQ6a9OmwPO5jBIK+qnD1G/+Ki/C9fU7/6Urrba8NxN9KPvWX0Q++btNMkqnc+z1X4r19a3Jr5ZIZJMNOkUBIzaatX5/m9vcsVhl95KfvIVad6Rt0tsQrL5d97l/r5Tzmp19b615jjH9zY3L9x3GZQ1HkR0ty6L+yEVJ93wVsCQkkhoTJ5pjrlfKx74jamF2+F2ZvGLQz14vrvXB88Mx0d9rh3tLy7WIwz2tbNQvJr9fDD56d+biN3Pubgj56+a567vfBmLJRxDKUWFCFhFFbr6+srACfdm1U7dn/abFV4rfS7PLQE3XL2zssD7+LnbnZD6Z2qfu+F6de74S04+/T1xd2j+Vd3l1xsoC1d24wKkuAjEqtMITIKASitkPLULHuQL97ofukr8syBvr7fTKfHuYLxoAJbTaHOw/E///C5twyPUK+JiXs3jwa1+sWn4zeOxRYls6jkDLIwi9JMRm1ubp5aYLLon72V3n+V8moD26Mr5/O9hX3fZZql/LN3Uqw2N42vipKjH6BjMphVApSCL7SQhJgSZYVjYkkgVsHUkNUma337yj49exiratz5UNeDQVX2ziUxCdVysfjpD1z9kavHmJ1pwsH+XqpK/7uvlL97IxkiBQyxsyQgLIBkssCoxuPx69eH78zLY17FWXNXVe/fjGPVSalnCz3O0nsuD+dt/83D2Kuq4Fa3e3lZuHyjUBxcZxSCJElJG+MjAGki1GBFjiSQIsOWlrymxuPh4gabeukTJjco865vNmnxY0+d//tPbkp7nPLRzu07ypgZ1P/q08FoZZGj7zl4a01iAZ0h6cSsRqPRvQMIJzBasBfy/guHg3VaPLJJx20orTluOe/2Pvim9YWLX7zZ5EVZVuVBrwMgt1Otlc3yrnekLWnjXZ9pREaihMoKKkUqMgGK5RaygfhlqdiJmjo4V8OPPko/9x7j21kqznqeaVzHgfpPn5x+pSmGyIyUOAkzKJOElLYpeo2sNjc37xX8SYWj4mZGdkTZJw/oXVv+HZvlK0t3ZrDeoJo24f3X6itZ89Wd5maXb9a66HfJVokhCDGStpl3Ls80cTSiWBYR15lYIbDXo7LJ2uNe1xQaVvmczbVB+Pjj+IGrVaPqLB/Mlvu+Jwfdl144/F/XpWRwoIA0KpMYIqgopI0JfVNarcbj8b2mf/LMUGiOPjcty43Dbnsg79iqGu8H63UuyPPDxy7ljz+0Nj/qbx300WgrPVoraEgAUpcpiEgt2SzNcpWr5GMErxTrEGPUypjIs2gU+h961P7U4/TmYVdkOjOmT45mO4bwT67Tf3uun3uDSqNSWqKkCMIoSaMAR61NZFCTyeSE9XtV4fNgNKnZbIvMQTf4/C6tbeQffXOEqE1yeV2DrfO4eOcWXjlbH3Xh1X6wcOhjykhyDBx6RrB5caRUREKQTIvVBGCd002re0N/8yH52bfFJ87hLOJxo0ublza1R3uuPPOnd/R//8Leji+Hw0nsZhh7UObkGn3vZVqtra3de6P7dpUhxMZRXeUGu2SzAylv7hwaxse38mCz4WiQuWYxnfXGbtb05KR7+xZcrhKITHuZSyY6B06xnVM26Tv2UUVQfd8Ynj92rvjAE2f/yaOLrao99uFgaeYLl3gumNoldqh++6Xut16G621OSkkKIfiqyO8t698LAK9du/bG0gAAGJ/r2hz3h0mF9cpCz6G1IZU//sTyb3zvW88O9dGNne1xxXz0mWdfvNsOJ9qzLRdR357F3YYXXCyxbBnVdH8wGIzqqjLpbB0fP6fPZS0f3/qyOzs98llqhvl8EexSPbQ+Gmxm+398M/uDF+eHtDYyyO0B2FpM5ZwzEFc35pTSydwWEeG1a9feKH4ASFjWsOyD77IJgVTuUJtyRutzn96/fvNnn5z8lUceCinePr51MF1otX79uHd9QxyGZTEclIUhxREljKoUXNTaKMrajmc9zx3OXeySRO2Drrgz5/Xywpp7aWZ+82v0tRktpFAaB3GqAXw2bqLG0GklJ1NnIYQTAN/uUt4HgMjPls1guGHJiHdK2z75ssIB+G/M1advpp394zUb1uuCfVoeHx16bQlLzYp98o34nlJv2O/5apnUzMuiT0m0snlAXrh2ZOdh5oeotsayTPET38Tf+Do92xrGLLdap54BJRt2EcC3Ix280H1l4Nfl/vDDD5/avswheTOYdW5sk45dz5APJvNlqzDlpCOoJqVN03/4gvngoxtr4yI/fi2SOujgThM8ZpuDesP0hZtV5XokCASIkJxv2pmHqKtsK+ENHlx3o+evL5/fcXckCzENiZPYDB2ANKy6CHVmKor9ch50cW+qltLrfe8HAmBmQsm0CSGwsmhsCEuDSQQFxzGJzV0fuG/o7EBt5UfvvLR5ZShvnsQ11Xrfd5FQVyYvFq4blMpwa2McFMNOqheP3MtHixe7zW/eOLg+5yaZUWEq7BcuhGKsIxp3iCip2kyJTVgYYxvJKXUnw1jMvBouAwC8du2a1nqFSWutlDpxkVPL6N/W3T2DpSJy1HGdqe0Szg/o/BDOl7yWS2VEwAdRTSoOnbmzxBvH6c7czbq4KnGuZq/uI/WgWS2ttYjEGE+UQER49erV1czl66XVb42O3t+6egOA+6jnGqNAYPEJPUMEtWpbFQjAQhBRGDCRYkQBAp3MG7sbpxK/zy5WsGOMqxG61zGtPPqExGoAEP4yqw0gIiDJoGRKjCIiUggLEmQBBkqIogFIgULBqMIb+3x/AferabIThCesaq01f2utUrpVqLp/rugN5ej7qCtSCABAIImEJcUUOAEbBiIC0qQ0k46AXpARTDqtRXnPm5PT6kT2q8nElf2szISI9EreK/Gv6lanDr6+UUj3bYDRCSGSBsoDYhJMiMJoCEUSioBEiQklobACFrL/n7K/D8DKP40xq+lRvRoCtNYS0co/Vhj+YnL3yun1vUkjgHAkiUqSAgAUAIgprZxUgSJADUSoCNTiW3yf6rtvpL9i6STArMQdY9Sj0Wg+n4cQVhhOJgP/gtHRB9koAAiYCMgEAiioACCPSQCBIBIAcM+RJTJzhvZUoTxIFauh4xPeVjGzrmt98eLFV199dblcrv60griKUH8pAFEIABCYJCkU5AQACJIIBUEYRQiESJRGjUhJ3H3h+P8Zf1bcr5Ag4nA4PHfu3P8FFg0JVGAqQoUAAAAldEVYdGRhdGU6Y3JlYXRlADIwMTctMDItMjFUMDQ6MzQ6NDkrMDE6MDDonuaJAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDE2LTExLTE3VDE3OjEwOjExKzAxOjAwQpxhegAAAABJRU5ErkJggg=="
