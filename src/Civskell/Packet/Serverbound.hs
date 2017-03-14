{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Civskell.Packet.Serverbound where

import Data.Int
import Data.Functor.Identity
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import Crypto.Hash (hash,Digest,SHA1)
import Numeric (showHex)
import Control.Monad
import Data.Attoparsec.ByteString hiding (take)
import qualified Data.Attoparsec.ByteString as P
import Data.SuchThat

import Civskell.Data.Types hiding (Player)
import Civskell.Tech.Parse
import Civskell.Tech.Network
import Civskell.Entity.Mob
import Civskell.Data.Player
import Civskell.Data.Logging
import Civskell.Data.World
import qualified Civskell.Packet.Clientbound as Client

  -- Protocol Version, Server Address, Server Port, Next State

parseHandshakePacket :: Parser (ServerPacket 'Handshaking) --(Packet p, PacketSide p ~ 'Server, PacketState p ~ 'Handshaking) => Parser p
parseHandshakePacket = choice [ambiguate . Identity <$> parsePacket @Handshake, ambiguate . Identity <$> parsePacket @LegacyHandshake] <?> "Handshake Packet"

parseLoginPacket :: Parser (ServerPacket 'LoggingIn)
parseLoginPacket = choice [ambiguate . Identity <$> parsePacket @LoginStart, ambiguate . Identity <$> parsePacket @EncryptionResponse] <?> "Login Packet"

parseStatusPacket :: Parser (ServerPacket 'Status)
parseStatusPacket = choice [ambiguate . Identity <$> parsePacket @StatusRequest, ambiguate . Identity <$> parsePacket @StatusPing] <?> "Status Packet"

-- TODO: SuchThat '[SP 'Playing] Parser
parsePlayPacket :: Parser (ServerPacket 'Playing)
parsePlayPacket = choice
  [ambiguate . Identity <$> parsePacket @TPConfirm
  ,ambiguate . Identity <$> parsePacket @ChatMessage
  ,ambiguate . Identity <$> parsePacket @ClientStatus
  ,ambiguate . Identity <$> parsePacket @ClientSettings
  ,ambiguate . Identity <$> parsePacket @ConfirmTransaction
  ,ambiguate . Identity <$> parsePacket @ClickWindow
  ,ambiguate . Identity <$> parsePacket @CloseWindow
  ,ambiguate . Identity <$> parsePacket @PluginMessage
  ,ambiguate . Identity <$> parsePacket @UseEntity
  ,ambiguate . Identity <$> parsePacket @KeepAlive
  ,ambiguate . Identity <$> parsePacket @PlayerPosition
  ,ambiguate . Identity <$> parsePacket @PlayerPositionAndLook
  ,ambiguate . Identity <$> parsePacket @PlayerLook
  ,ambiguate . Identity <$> parsePacket @Player
  ,ambiguate . Identity <$> parsePacket @PlayerAbilities
  ,ambiguate . Identity <$> parsePacket @PlayerDigging
  ,ambiguate . Identity <$> parsePacket @EntityAction
  ,ambiguate . Identity <$> parsePacket @HeldItemChange
  ,ambiguate . Identity <$> parsePacket @CreativeInventoryAction
  ,ambiguate . Identity <$> parsePacket @Animation
  ,ambiguate . Identity <$> parsePacket @PlayerBlockPlacement
  ,ambiguate . Identity <$> parsePacket @UseItem
  ] <?> "Play Packet"

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
  parsePacket = do
    specificVarInt 0x00
    Handshake
      <$> parseVarInt
      <*> parseVarString
      <*> parseUnsignedShort
      <*> parseVarInt

data LegacyHandshake = LegacyHandshake
instance Packet LegacyHandshake where
  type PacketSide LegacyHandshake = 'Server
  type PacketState LegacyHandshake = 'Handshaking
  packetName = "LegacyHandshake"
  packetId = 0xFE
  packetPretty LegacyHandshake = []
  parsePacket = do
    _ <- word8 0xFE
    _ <- takeByteString
    return LegacyHandshake

data TPConfirm = TPConfirm VarInt
instance Packet TPConfirm where
  type PacketSide TPConfirm = 'Server
  type PacketState TPConfirm = 'Playing
  packetName = "TPConfirm"
  packetId = 0x00
  packetPretty (TPConfirm i) = [("Teleport Id",show i)]
  -- Check the tid presented against all the tid's we have stored
  onPacket (TPConfirm tid) = clearTeleport tid >>= \case
    -- If it's valid, say so
    True -> logp $ "Client confirms teleport with id: " ++ show tid
    -- If it's not, complain
    False -> loge $ "Client provided bad teleport id: " ++ show tid
  parsePacket = do
    specificVarInt 0x00 <?> "Packet Id 0x00"
    TPConfirm <$> parseVarInt

data TabComplete = TabComplete
instance Packet TabComplete where
  type PacketSide TabComplete = 'Server
  type PacketState TabComplete = 'Playing
  packetName = "TabComplete"
  packetId = 0x01
  packetPretty (TabComplete) = []
  parsePacket = error "No parser for packet"

data ChatMessage = ChatMessage String
instance Packet ChatMessage  where
  type PacketSide ChatMessage = 'Server
  type PacketState ChatMessage = 'Playing
  packetName = "ChatMessage "
  packetId = 0x02
  packetPretty (ChatMessage msg) = [("Message",msg)]
  onPacket (ChatMessage msg) = case msg of
    "/gamemode 1" -> setGamemode Creative
    "/gamemode 0" -> setGamemode Survival
    "chunks" -> forM_ [0..48] $ \x -> sendPacket =<< colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
    "creeper" -> summonMob (Creeper defaultInsentient 0 False False)
    _ -> do
      broadcastPacket (Client.ChatMessage (jsonyText msg) 0)
      name <- getUsername
      logt name msg

  parsePacket = do
    specificVarInt 0x02 <?> "Packet Id 0x02"
    ChatMessage <$> parseVarString

data ClientStatus = ClientStatus ClientStatusAction
instance Packet ClientStatus where
  type PacketSide ClientStatus = 'Server
  type PacketState ClientStatus = 'Playing
  packetName = "ClientStatus"
  packetId = 0x03
  packetPretty (ClientStatus status) = [("Status",show status)]
  onPacket (ClientStatus status) = case status of
    PerformRespawn -> logp "Client wants to perform respawn"
    RequestStats -> logp "Client requests stats"
    OpenInventory -> logp "Client is opening their inventory"
  parsePacket = do
    specificVarInt 0x03 <?> "Packet Id 0x03"
    ClientStatus <$> choice
      [specificVarInt 0x00 *> pure PerformRespawn
      ,specificVarInt 0x01 *> pure RequestStats
      ,specificVarInt 0x02 *> pure OpenInventory
      ]

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
  -- Teleport the client when they send this packet because reasons
  -- 0x00 means all absolute (It's a relativity flag bitfield)
  onPacket (ClientSettings _loc _viewDist _chatMode _chatColors _skin _hand) = pendTeleport (1.0,130.0,1.0) (0.0,0.0) 0x00
  parsePacket = do
    specificVarInt 0x04 <?> "Packet Id 0x04"
    ClientSettings
      <$> parseVarString
      <*> anyWord8
      <*> parseVarInt
      <*> parseBool
      <*> anyWord8
      <*> parseVarInt

data ConfirmTransaction = ConfirmTransaction WindowId TransactionId Bool
instance Packet ConfirmTransaction where
  type PacketSide ConfirmTransaction = 'Server
  type PacketState ConfirmTransaction = 'Playing
  packetName = "ConfirmTransaction"
  packetId = 0x05
  packetPretty (ConfirmTransaction wid transId acc) = [("Window Id",show wid),("Transaction Id",show transId),("Accepted",if acc then "Yes" else "No")]
  parsePacket = do
    specificVarInt 0x05 <?> "Packet Id 0x05"
    ConfirmTransaction
      <$> parseWID
      <*> parseShort
      <*> parseBool

data EnchantItem = EnchantItem
instance Packet EnchantItem where
  type PacketSide EnchantItem = 'Server
  type PacketState EnchantItem = 'Playing
  packetName = "EnchantItem"
  packetId = 0x06
  packetPretty (EnchantItem) = []
  parsePacket = error "No parser for packet"

-- Slot Number
data ClickWindow = ClickWindow WindowId Short TransactionId InventoryClickMode Slot
instance Packet ClickWindow where
  type PacketSide ClickWindow = 'Server
  type PacketState ClickWindow = 'Playing
  packetName = "ClickWindow"
  packetId = 0x07
  packetPretty (ClickWindow wid slotNum transId invMode item) = [("Window Id",show wid),("Slot Number",show slotNum),("Transaction Id",show transId),("Inventory Mode",show invMode),("Subject Item",show item)]
  onPacket (ClickWindow wid slotNum transId mode clientProvidedSlot) = if wid /= 0 then loge "Non-player inventories not supported" else case mode of
    -- TODO: optimize and combine cases; right-click usually just means to move one item, but we consider it an entirely separate case here
    NormalClick rClick -> do
      -- Get the current state of affairs, and purely decide what to do with them
      doInventoryClick <$> getInventorySlot (clientToCivskellSlot slotNum) <*> getInventorySlot (-1) <*> pure rClick <*> pure clientProvidedSlot >>= \case
        -- If everything is in order
        Just (newSlot,newHand) -> do
          -- Set the slots to their new values
          setInventorySlot (-1) newHand
          setInventorySlot (clientToCivskellSlot slotNum) newSlot
          -- Confirm the transaction was successful
          sendPacket (Client.ConfirmTransaction wid transId True)
        -- If something went wrong
        Nothing -> do
          -- Log to console
          loge "Failed to confirm client transaction"
          -- And tell the client it should say sorry
          sendPacket (Client.ConfirmTransaction wid transId False)
      -- Right click is exactly the same as left when shiftclicking
    -- TODO: implement the rest of the clicking bs that minecraft does
    ShiftClick _rClick -> pure ()
    NumberKey _num -> pure ()
    MiddleClick -> pure ()
    ItemDropOut _isStack -> pure ()
    -- "Painting" mode
    PaintingMode _mode -> loge "Painting mode not supported"
    -- Double click
    DoubleClick -> loge "Double click not supported"

  parsePacket = do
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
    sl <- parseSlot
    return $ ClickWindow wid slotNum transId mode sl

doInventoryClick :: Slot -> Slot -> Bool -> Slot -> Maybe (Slot,Slot)
doInventoryClick actualSlot currHeld rClick shouldBeSlot = if actualSlot /= shouldBeSlot then Nothing else case actualSlot of
  EmptySlot -> case currHeld of
    -- Both empty -> No-op
    EmptySlot -> Just (actualSlot,currHeld)
    -- Putting something in an empty slot
    Slot currbid currcount currdmg currnbt -> Just (placed,newHeld)
      where
        -- If we right click, only place one item, left click places all of them
        delta = if rClick then 1 else 64
        -- If we don't have enough items to fill the delta, place them all and end up with an EmptySlot, otherwise remove the delta
        newHeld = if currcount <= delta then EmptySlot else Slot currbid (currcount - delta) currdmg currnbt
        -- The slot now has either the full delta, or our best attempt at filling the delta
        placed = Slot currbid (min delta currcount) currdmg currnbt
  Slot actbid actcount actdmg actnbt -> case currHeld of
    -- Picking something up into an empty hand
    EmptySlot -> Just (left,picked)
      where
        -- If we right click, take half, otherwise take as much as we can
        delta = if rClick then actcount `div` 2 else min actcount 64
        -- If we took it all, leave nothing, otherwise take what we took
        left = if actcount == delta then EmptySlot else Slot actbid (actcount - delta) actdmg actnbt
        -- We are now holding everything we picked up
        picked = Slot actbid delta actdmg actnbt
    -- Two item stacks interacting
    Slot currbid currcount currdmg currnbt -> case currbid == actbid && currdmg == actdmg && currnbt == actnbt of
      -- Like stacks; combine
      True -> Just (inSlot,stillHeld)
        where
          -- How many items we can possibly place in the slot
          spaceRemaining = 64 - actcount
          -- If we right click, try to put one in, but put zero in if its already full, otherwise put as many as we can in
          -- NOTE: delta <= currcount and spaceRemaining
          delta = if rClick then min 1 spaceRemaining else max currcount spaceRemaining
          -- If we put down everything, empty our hand, otherwise remove what we put down
          stillHeld = if currcount == delta then EmptySlot else Slot currbid (currcount - delta) currdmg currnbt
          -- Put the stuff we put into the slot, into the slot
          inSlot = Slot actbid (actcount + delta) actdmg actnbt
      -- Unlike stacks; swap
      False -> Just (currHeld,actualSlot)

data CloseWindow = CloseWindow WindowId
instance Packet CloseWindow where
  type PacketSide CloseWindow = 'Server
  type PacketState CloseWindow = 'Playing
  packetName = "CloseWindow"
  packetId = 0x08
  packetPretty (CloseWindow wid) = [("Window Id", show wid)]
  onPacket (CloseWindow wid) = logp $ "Player is closing a window with id: " ++ show wid
  parsePacket = do
    specificVarInt 0x08 <?> "Packet Id 0x08"
    CloseWindow <$> parseWID

data PluginMessage = PluginMessage String BS.ByteString
instance Packet PluginMessage where
  type PacketSide PluginMessage = 'Server
  type PacketState PluginMessage = 'Playing
  packetName = "PluginMessage"
  packetId = 0x09
  packetPretty (PluginMessage "MC|Brand" cliBrand) = [("Client Brand",show (BS.tail cliBrand))]
  packetPretty (PluginMessage chan bs) = [("Channel",show chan),("Payload",show bs)]
  -- BS.tail removes the length prefixing
  onPacket (PluginMessage "MC|Brand" cliBrand) = setBrand $ show (BS.tail cliBrand)
  onPacket p = logp $ "Unsupported Plugin Message: " ++ showPacket p
  parsePacket = do
    specificVarInt 0x09 <?> "Packet Id 0x09"
    PluginMessage
      <$> parseVarString
      <*> takeByteString

data UseEntity = UseEntity EntityId EntityInteraction
instance Packet UseEntity where
  type PacketSide UseEntity = 'Server
  type PacketState UseEntity = 'Playing
  packetName = "UseEntity"
  packetId = 0x0A
  packetPretty (UseEntity targetEID action) = [("Target",show targetEID),("Action",show action)]
  onPacket (UseEntity targetEID action) = do
    (SuchThat (Identity (_ :: m))) <- getEntity targetEID
    logg $ entityName @m ++ " was " ++ show action ++ "(ed)"
  parsePacket = do
    specificVarInt 0x0A <?> "Packet Id 0x0A"
    UseEntity
      <$> parseEID
      <*> choice
        [specificVarInt 0x00 *> (Interact <$> parseHand)
        ,specificVarInt 0x01 *> pure Attack
        ,specificVarInt 0x02 *> (InteractAt <$> ((,,) <$> parseFloat <*> parseFloat <*> parseFloat) <*> parseHand)
        ]

data KeepAlive = KeepAlive KeepAliveId
instance Packet KeepAlive where
  type PacketSide KeepAlive = 'Server
  type PacketState KeepAlive = 'Playing
  packetName = "KeepAlive"
  packetId = 0x0B
  packetPretty (KeepAlive i) = [("Keep Alive Id",show i)]
  onPacket (KeepAlive kid) = logp $ "Player sent keep alive pong with id: " ++ show kid
  parsePacket  = do
    specificVarInt 0x0B <?> "Packet Id 0x0B"
    KeepAlive <$> parseVarInt

data PlayerPosition = PlayerPosition (Double,Double,Double) Bool
instance Packet PlayerPosition where
  type PacketSide PlayerPosition = 'Server
  type PacketState PlayerPosition = 'Playing
  packetName = "PlayerPosition"
  packetId = 0x0C
  packetPretty (PlayerPosition (x,y,z) grounded) = [("Positon",show (x,y,z)),("On Ground",show grounded)]
  onPacket (PlayerPosition (x,y,z) _grounded) = setPlayerPos (x,y,z)
  parsePacket = do
    specificVarInt 0x0C <?> "Packet Id 0x0C"
    PlayerPosition
      <$> ((,,) <$> parseDouble <*> parseDouble <*> parseDouble)
      <*> parseBool

data PlayerPositionAndLook = PlayerPositionAndLook (Double,Double,Double) (Float,Float) Bool
instance Packet PlayerPositionAndLook where
  type PacketSide PlayerPositionAndLook = 'Server
  type PacketState PlayerPositionAndLook = 'Playing
  packetName = "PlayerPositionAndLook"
  packetId = 0x0D
  packetPretty (PlayerPositionAndLook (x,y,z) (yaw,pitch) grounded) = [("Positon",show (x,y,z)),("Looking",show (yaw,pitch)),("On Ground",show grounded)]
  onPacket (PlayerPositionAndLook (x,y,z) (yaw,pitch) _grounded) = do
    setPlayerPos (x,y,z)
    setPlayerViewAngle (yaw,pitch)
  parsePacket = do
    specificVarInt 0x0D <?> "Packet Id 0x0D"
    PlayerPositionAndLook
      <$> ((,,) <$> parseDouble <*> parseDouble <*> parseDouble)
      <*> ((,) <$> parseFloat <*> parseFloat)
      <*> parseBool

data PlayerLook = PlayerLook (Float,Float) Bool
instance Packet PlayerLook where
  type PacketSide PlayerLook = 'Server
  type PacketState PlayerLook = 'Playing
  packetName = "PlayerLook"
  packetId = 0x0E
  packetPretty (PlayerLook  (yaw,pitch) grounded) = [("Looking",show (yaw,pitch)),("On Ground",show grounded)]
  onPacket (PlayerLook (y,p) _grounded) = setPlayerViewAngle (y,p)
  parsePacket = do
    specificVarInt 0x0E <?> "Packet Id 0x0E"
    PlayerLook
      <$> ((,) <$> parseFloat <*> parseFloat)
      <*> parseBool

data Player = Player Bool
instance Packet Player where
  type PacketSide Player = 'Server
  type PacketState Player = 'Playing
  packetName = "Player"
  packetId = 0x0F
  packetPretty (Player grounded) = [("On Ground",show grounded)]
  onPacket (Player _grounded) = pure ()
  parsePacket = do
    specificVarInt 0x0F <?> "Packet Id 0x0F"
    Player <$> parseBool

data VehicleMove = VehicleMove
instance Packet VehicleMove where
  type PacketSide VehicleMove = 'Server
  type PacketState VehicleMove = 'Playing
  packetName = "VehicleMove"
  packetId = 0x10
  packetPretty (VehicleMove) = []
  parsePacket = error "No parser for packet"

data SteerBoat = SteerBoat
instance Packet SteerBoat where
  type PacketSide SteerBoat = 'Server
  type PacketState SteerBoat = 'Playing
  packetName = "SteerBoat"
  packetId = 0x11
  packetPretty (SteerBoat) = []
  parsePacket = error "No parser for packet"

data PlayerAbilities = PlayerAbilities AbilityFlags Float Float
instance Packet PlayerAbilities where
  type PacketSide PlayerAbilities = 'Server
  type PacketState PlayerAbilities = 'Playing
  packetName = "PlayerAbilities"
  packetId = 0x12
  packetPretty (PlayerAbilities (AbilityFlags i f af c) flySpeed fovMod) = u i "Invulnerable" ++ u f "Flying" ++ u af "Allow Flying" ++ u c "Creative" ++ [("Flying Speed",show flySpeed),("FOV Modifier",show fovMod)]
    where
      u b s = if b then [(s,"")] else []
  -- Only sent when flight is toggled
  onPacket (PlayerAbilities (AbilityFlags _i f _af _c) _flySpeed _fovMod) = if f then setMoveMode Flying else setMoveMode Walking
  parsePacket = do
    specificVarInt 0x12 <?> "Packet Id 0x12"
    PlayerAbilities <$> parseFlags <*> parseFloat <*> parseFloat
    where
      parseFlags = do
        flags <- anyWord8
        pure (AbilityFlags (testBit flags 0) (testBit flags 1) (testBit flags 2) (testBit flags 3))

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
  onPacket p@(PlayerDigging action) = case action of
    StartDig block _side -> do
      logp $ "Started digging block: " ++ show block
      -- Instant Dig
      removeBlock block
    SwapHands -> do
      -- Get the current items
      heldSlot <- getHolding
      heldItem <- getInventorySlot heldSlot
      -- 45 is the off hand slot
      offItem <- getInventorySlot 45
      -- Swap them
      setInventorySlot heldSlot offItem
      setInventorySlot 45 heldItem
    -- No dropping from offhand
    DropItem isStack -> do
      heldSlot <- getHolding
      heldItem <- getInventorySlot heldSlot
      -- Drop the entire stack, or at most one item
      let (dropped,newHeld) = splitStack (if isStack then 64 else 1) heldItem
      setInventorySlot heldSlot newHeld
      summonObject (Item (BaseEntity (EntityLocation (0,130,0) (0,0)) (EntityVelocity (0,0,0)) 0x00 300 "" False False False) dropped)
    _ -> loge $ "Unhandled Player Dig Action: " ++ showPacket p

  parsePacket = do
    specificVarInt 0x13 <?> "Packet Id 0x13"
    PlayerDigging <$> choice
      [specificVarInt 0x00 *> (StartDig <$> parseBlockCoord <*> parseBlockFace)
      ,specificVarInt 0x01 *> (StopDig <$> parseBlockCoord <*> parseBlockFace)
      ,specificVarInt 0x02 *> (EndDig <$> parseBlockCoord <*> parseBlockFace)
      ,specificVarInt 0x03 *> trashExtra (pure (DropItem True))
      ,specificVarInt 0x04 *> trashExtra (pure (DropItem False))
      ,specificVarInt 0x05 *> trashExtra (pure ShootArrowOrFinishEating)
      ,specificVarInt 0x06 *> trashExtra (pure SwapHands)
      ]
    where
      trashExtra = (<*(parseBlockCoord <* parseBlockFace))

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
  -- We know the eid because its us
  onPacket (EntityAction _eid action) = case action of
    Sneak True -> setMoveMode Sneaking
    Sneak False -> setMoveMode Walking
    Sprint True -> setMoveMode Sprinting
    Sprint False -> setMoveMode Walking
    HorseJump _ _ -> logp "Jumping with horse"
    LeaveBed -> pure ()
    HorseInventory -> pure () -- Open window here?
    ElytraFly -> pure () -- ?
  parsePacket = do
    specificVarInt 0x14 <?> "Packet Id 0x14"
    EntityAction <$> parseEID <*> choice
      [specificVarInt 0x00 *> pure (Sneak True)
      ,specificVarInt 0x01 *> pure (Sneak False)
      ,specificVarInt 0x02 *> pure LeaveBed
      ,specificVarInt 0x03 *> pure (Sprint True)
      ,specificVarInt 0x04 *> pure (Sprint False)
      ,specificVarInt 0x05 *> (HorseJump True <$> parseVarInt)
      ,specificVarInt 0x06 *> (HorseJump False <$> parseVarInt)
      ,specificVarInt 0x07 *> pure HorseInventory
      ,specificVarInt 0x08 *> pure ElytraFly
      ]

data SteerVehicle = SteerVehicle
instance Packet SteerVehicle where
  type PacketSide SteerVehicle = 'Server
  type PacketState SteerVehicle = 'Playing
  packetName = "SteerVehicle"
  packetId = 0x15
  packetPretty (SteerVehicle) = []
  parsePacket = error "No parser for packet"

data ResourcePackStatus = ResourcePackStatus
instance Packet ResourcePackStatus where
  type PacketSide ResourcePackStatus = 'Server
  type PacketState ResourcePackStatus = 'Playing
  packetName = "ResourcePackStatus"
  packetId = 0x16
  packetPretty (ResourcePackStatus) = []
  parsePacket = error "No parser for packet"

data HeldItemChange = HeldItemChange Short
instance Packet HeldItemChange where
  type PacketSide HeldItemChange = 'Server
  type PacketState HeldItemChange = 'Playing
  packetName = "HeldItemChange"
  packetId = 0x17
  packetPretty (HeldItemChange i) = [("Slot",show i)]
  -- Update the slot they are holding in the player data
  onPacket (HeldItemChange slotNum) = setHolding slotNum
  parsePacket = do
    specificVarInt 0x17 <?> "Packet Id 0x17"
    HeldItemChange <$> parseShort

data CreativeInventoryAction = CreativeInventoryAction Short Slot
instance Packet CreativeInventoryAction where
  type PacketSide CreativeInventoryAction = 'Server
  type PacketState CreativeInventoryAction = 'Playing
  packetName = "CreativeInventoryAction"
  packetId = 0x18
  packetPretty (CreativeInventoryAction slot item) = [("Slot",show slot),("New Item", show item)]
  -- Clients handle all the dirty details such that this is just a "set slot" packet
  onPacket (CreativeInventoryAction slotNum slotDat) = do
    logp $ "Player creatively set slot " ++ show slotNum ++ " to {" ++ show slotDat ++ "}"
    -- TODO: This will echo back a SetSlot packet, add another way to access the effect inventory
    -- Maybe that is ok? idk requires further testing to be sure
    setInventorySlot (clientToCivskellSlot slotNum) slotDat
  parsePacket = do
    specificVarInt 0x18 <?> "Packet Id 0x18"
    CreativeInventoryAction <$> parseShort <*> parseSlot

data UpdateSign = UpdateSign
instance Packet UpdateSign where
  type PacketSide UpdateSign = 'Server
  type PacketState UpdateSign = 'Playing
  packetName = "UpdateSign"
  packetId = 0x19
  packetPretty (UpdateSign) = []
  parsePacket = error "No parser for packet"

data Animation = Animation Hand
instance Packet Animation where
  type PacketSide Animation = 'Server
  type PacketState Animation = 'Playing
  packetName = "Animation"
  packetId = 0x1A
  packetPretty (Animation hand) = [("Hand",show hand)]
  -- Don't do anything about the spammy animation packets
  onPacket (Animation _anim) = return ()
  parsePacket = do
    specificVarInt 0x1A <?> "Packet Id 0x1A"
    Animation <$> parseHand

data Spectate = Spectate
instance Packet Spectate where
  type PacketSide Spectate = 'Server
  type PacketState Spectate = 'Playing
  packetName = "Spectate"
  packetId = 0x1B
  packetPretty (Spectate) = []
  parsePacket = error "No parser for packet"

data PlayerBlockPlacement = PlayerBlockPlacement BlockCoord BlockFace Hand (Float,Float,Float)
instance Packet PlayerBlockPlacement where
  type PacketSide PlayerBlockPlacement = 'Server
  type PacketState PlayerBlockPlacement = 'Playing
  packetName = "PlayerBlockPlacement"
  packetId = 0x1C
  packetPretty (PlayerBlockPlacement block _side hand _cursorCoord) = [("Block",show block),("Hand",show hand)]
  onPacket (PlayerBlockPlacement block side hand _cursorCoord) = do
    -- Find out what item they are trying to place
    heldSlot <- if hand == MainHand then getHolding else pure 45
    heldItem <- getInventorySlot heldSlot
    case heldItem of
      -- If they right click on a block with an empty hand, this will happen
      EmptySlot -> logp "Trying to place air"
      Slot bid icount dmg nbt -> do
        -- Remove item from inventory
        let newSlot = if icount == 1 then EmptySlot else (Slot bid (icount - 1) dmg nbt)
        setInventorySlot heldSlot newSlot
        -- Updates client as well
        -- TODO: map item damage to block damage somehow
        setBlock (BlockState bid 0) (blockOnSide block side)
  parsePacket = do
    specificVarInt 0x1C <?> "Packet Id 0x1C"
    PlayerBlockPlacement
      <$> parseBlockCoord
      <*> parseBlockFace
      <*> parseHand
      <*> ((,,) <$> parseFloat <*> parseFloat <*> parseFloat)

data UseItem = UseItem Hand
instance Packet UseItem where
  type PacketSide UseItem = 'Server
  type PacketState UseItem = 'Playing
  packetName = "UseItem"
  packetId = 0x1D
  packetPretty (UseItem hand) = [("Hand",show hand)]
  onPacket (UseItem hand) = do
    held <- getInventorySlot =<< (case hand of {MainHand -> getHolding; OffHand -> pure 45})
    logp $ "Used: " ++ show held
  parsePacket = do
    specificVarInt 0x1D <?> "Packet Id 0x1D"
    UseItem <$> parseHand

data LoginStart = LoginStart String
instance Packet LoginStart where
  type PacketSide LoginStart = 'Server
  type PacketState LoginStart = 'LoggingIn
  packetName = "LoginStart"
  packetId = 0x00
  packetPretty (LoginStart name) = [("Username",name)]
  parsePacket = do
    specificVarInt 0x00
    LoginStart <$> parseVarString

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
  parsePacket = do
    specificVarInt 0x01
    ssLen <- fromEnum <$> parseVarInt
    ss <- P.take ssLen
    vtLen <- fromEnum <$> parseVarInt
    vt <- P.take vtLen
    return $ EncryptionResponse ss vt

data StatusRequest = StatusRequest
instance Packet StatusRequest where
  type PacketSide StatusRequest = 'Server
  type PacketState StatusRequest = 'Status
  packetName = "StatusRequest"
  packetId = 0x00
  packetPretty StatusRequest = []
  parsePacket = try (specificVarInt 0x00 >> endOfInput) >> return StatusRequest

data StatusPing = StatusPing Int64
instance Packet StatusPing where
  type PacketSide StatusPing = 'Server
  type PacketState StatusPing = 'Status
  packetName = "StatusPing"
  packetId = 0x01
  packetPretty (StatusPing i) = [("Ping Token","0x" ++ showHex i "")]
  parsePacket = do
    specificVarInt 0x01
    StatusPing <$> parseLong

