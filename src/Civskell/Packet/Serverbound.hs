{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Civskell.Packet.Serverbound where

import Data.Int
import Data.List (intercalate)
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
import Civskell.Tech.Encrypt
import Civskell.Entity.Mob
import Civskell.Data.Player
import Civskell.Data.Logging
import Civskell.Data.World
import qualified Civskell.Packet.Clientbound as Client

  -- Protocol Version, Server Address, Server Port, Next State

parseHandshakePacket :: Parser (ServerPacket 'Handshaking) --(Packet p, PacketSide p ~ 'Server, PacketState p ~ 'Handshaking) => Parser p
parseHandshakePacket = choice [ServerPacket . ambiguate . Identity <$> parsePacket @Handshake, ServerPacket . ambiguate . Identity <$> parsePacket @LegacyHandshake] <?> "Handshake Packet"

parseLoginPacket :: Parser (ServerPacket 'LoggingIn)
parseLoginPacket = choice [ServerPacket . ambiguate . Identity <$> parsePacket @LoginStart, ServerPacket . ambiguate . Identity <$> parsePacket @EncryptionResponse] <?> "Login Packet"

parseStatusPacket :: Parser (ServerPacket 'Status)
parseStatusPacket = choice [ServerPacket . ambiguate . Identity <$> parsePacket @StatusRequest, ServerPacket . ambiguate . Identity <$> parsePacket @StatusPing] <?> "Status Packet"

-- TODO: SuchThat '[SP 'Playing] Parser
parsePlayPacket :: Parser (ServerPacket 'Playing)
parsePlayPacket = choice
  [ServerPacket . ambiguate . Identity <$> parsePacket @TPConfirm
  ,ServerPacket . ambiguate . Identity <$> parsePacket @ChatMessage
  ,ServerPacket . ambiguate . Identity <$> parsePacket @ClientStatus
  ,ServerPacket . ambiguate . Identity <$> parsePacket @ClientSettings
  ,ServerPacket . ambiguate . Identity <$> parsePacket @ConfirmTransaction
  ,ServerPacket . ambiguate . Identity <$> parsePacket @ClickWindow
  ,ServerPacket . ambiguate . Identity <$> parsePacket @CloseWindow
  ,ServerPacket . ambiguate . Identity <$> parsePacket @PluginMessage
  ,ServerPacket . ambiguate . Identity <$> parsePacket @UseEntity
  ,ServerPacket . ambiguate . Identity <$> parsePacket @KeepAlive
  ,ServerPacket . ambiguate . Identity <$> parsePacket @PlayerPosition
  ,ServerPacket . ambiguate . Identity <$> parsePacket @PlayerPositionAndLook
  ,ServerPacket . ambiguate . Identity <$> parsePacket @PlayerLook
  ,ServerPacket . ambiguate . Identity <$> parsePacket @Player
  ,ServerPacket . ambiguate . Identity <$> parsePacket @PlayerAbilities
  ,ServerPacket . ambiguate . Identity <$> parsePacket @PlayerDigging
  ,ServerPacket . ambiguate . Identity <$> parsePacket @EntityAction
  ,ServerPacket . ambiguate . Identity <$> parsePacket @HeldItemChange
  ,ServerPacket . ambiguate . Identity <$> parsePacket @CreativeInventoryAction
  ,ServerPacket . ambiguate . Identity <$> parsePacket @Animation
  ,ServerPacket . ambiguate . Identity <$> parsePacket @PlayerBlockPlacement
  ,ServerPacket . ambiguate . Identity <$> parsePacket @UseItem
  ] <?> "Play Packet"

-- TODO: Re-specify parsers for different server states

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
  -- Normal handshake recieved
  onPacket (Handshake protocol _addr _port newstate) = if fromIntegral protocol == protocolVersion
    -- They are using the correct protocol version, so continue as they request
    -- TODO: Enum for newstate
    then case newstate of
      1 -> setPlayerState Status
      2 -> setPlayerState LoggingIn
      _ -> logp "Invalid newstate"
    -- They are using the incorrect protocol version. Disconnect packet will probably work even if they have a different version.
    else sendPacket (Client.Disconnect (jsonyText $ "Unsupported protocol version. Please use " ++ show protocolVersion))
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
  -- Legacy response
  onPacket LegacyHandshake = iSolemnlySwearIHaveNoIdeaWhatImDoing . serialize $ Client.LegacyHandshakePong
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
    "dump entities" -> summonMob (Creeper defaultInsentient 0 False False)
    _ -> do
      broadcastPacket (Client.ChatMessage (jsonyText msg) 0)
      name <- clientUsername <$> getPlayer
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
  -- TODO: check if the slot matches the client provided one
  onPacket (ClickWindow wid slotNum transId mode clientProvidedSlot) = if wid /= 0 then loge "Non-player inventories not supported" else case mode of
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
      heldSlot <- holdingSlot <$> getPlayer
      heldItem <- getInventorySlot heldSlot
      -- 45 is the off hand slot
      offItem <- getInventorySlot 45
      -- Swap them
      setInventorySlot heldSlot offItem
      setInventorySlot 45 heldItem
    -- No dropping from offhand
    DropItem isStack -> do
      heldSlot <- holdingSlot <$> getPlayer
      heldItem <- getInventorySlot heldSlot
      -- Drop the entire stack, or at most one item
      let (dropped,newHeld) = splitStack (if isStack then 64 else 1) heldItem
      setInventorySlot heldSlot newHeld
      plaLoc <- playerPosition <$> getPlayer
      summonObject (Item (BaseEntity (EntityLocation plaLoc (0,0)) (EntityVelocity (0,0,0)) 0x00 300 "" False False False) dropped)
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
    heldSlot <- if hand == MainHand then holdingSlot <$> getPlayer else pure 45
    heldItem <- getInventorySlot heldSlot
    case heldItem of
      -- If they right click on a block with an empty hand, this will happen
      EmptySlot -> logp "Trying to place air"
      Slot bid icount dmg nbt -> do
        -- Remove item from inventory
        let newSlot = if icount == 1 then EmptySlot else (Slot bid (icount - 1) dmg nbt)
        setInventorySlot heldSlot newSlot
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
    -- Decide which slot they are using, and find the item in that hand
    held <- getInventorySlot =<< (case hand of {MainHand -> holdingSlot <$> getPlayer; OffHand -> pure 45})
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
  -- Do the login process
  -- LoginStart packets contain their username as a String
  onPacket (LoginStart name) = do
    -- Log that they are logging in
    logt name "Logging In"
    setUsername name
    -- Verify Token is fixed because why not
    -- TODO: make this a random token
    let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
    -- Server Id is blank because (((history)))
    let sId = ""
    -- Send an encryption request to the client
    sendPacket (Client.EncryptionRequest sId encodedPublicKey vt)
    -- TODO
    --setPlayerState (AwaitingEncryptionResponse vt sId)
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
  -- Wait for them to send an Encryption Response
  onPacket (EncryptionResponse ssFromClient vtFromClient) = do
    -- TODO: get this from getPlayerState >>= \(AwaitingEcnryptionResponse vt sId) ->
    let vt = BS.pack [0xDE,0xAD,0xBE,0xEF]
    -- Make sure that the encryption stuff all lines up properly
    case checkVTandSS (snd globalKeypair) vtFromClient ssFromClient vt of
      -- If it doesn't, disconnect
      Left s -> sendPacket (Client.Disconnect (jsonyText s)) >> loge s
      -- If it does, keep going
      Right ss -> do
        -- Start encrypting our packets, now that we have the shared secret
        beginEncrypting ss
        -- Make the serverId hash for auth
        let loginHash = genLoginHash "" ss encodedPublicKey
        -- Do the Auth stuff with Mojang
        -- TODO: This uses arbitrary IO, we should make it into an effect
        name <- clientUsername <$> getPlayer
        authGetReq name loginHash >>= \case
          -- TODO: we just crash if the token is negative :3 pls fix or make it a feature
          -- If the auth is borked, its probably Mojangs fault tbh
          Left actualJSON -> do
            loge "Parse error on auth"
            -- Disclaim guilt
            sendPacket (Client.Disconnect $ jsonyText "Auth failed (not Lazersmoke's fault, probably!)")
            loge actualJSON
          Right (AuthPacket uuid nameFromAuth authProps) -> do
            pid <- registerPlayer
            setUsername nameFromAuth
            setUUID uuid
            -- Tell the client to compress starting now. Agressive for testing
            -- EDIT: setting this to 3 gave us a bad frame exception :S
            sendPacket (Client.SetCompression 16)
            beginCompression 16
            -- Send a login success. We are now in play mode
            sendPacket (Client.LoginSuccess (show uuid) nameFromAuth)
            -- This is where the protocol specifies the state transition to be
            setPlayerState Playing
            -- 100 is the max players
            sendPacket (Client.JoinGame pid Survival Overworld Peaceful 100 "default" False)
            -- Also sends player abilities
            setGamemode Survival
            -- Tell them we aren't vanilla so no one gets mad at mojang for our fuck ups
            sendPacket (Client.PluginMessage "MC|Brand" (serialize "Civskell"))
            -- Difficulty to peaceful
            sendPacket (Client.ServerDifficulty Peaceful)
            -- World Spawn/Compass Direction, not where they will spawn initially
            sendPacket (Client.SpawnPosition (Block (0,64,0)))
            -- Send initial world. Need a 7x7 grid or the client gets angry with us
            forM_ [0..48] $ \x -> sendPacket =<< colPacket ((x `mod` 7)-3,(x `div` 7)-3) (Just $ BS.replicate 256 0x00)
            -- Send an initial blank inventory
            sendPacket (Client.WindowItems 0 (replicate 45 EmptySlot))
            -- Give them some stone (for testing)
            setInventorySlot 4 (Slot 1 32 0 Nothing)
            ps <- allPlayers
            sendPacket (Client.PlayerListItem (map (\p -> (clientUUID p,PlayerListAdd (clientUsername p) authProps Survival 0 Nothing)) ps))
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
  onPacket _ = do
    -- Get the list of connected players so we can show info about them in the server menu
    playersOnline <- allPlayers
    let userSample = intercalate "," . map (\p -> "{\"name\":\"" ++ clientUsername p ++ "\",\"id\":\"" ++ show (clientUUID p) ++ "\"}") $ playersOnline
    let resp = "{\"version\":{\"name\":\"Civskell (1.11.2)\",\"protocol\":" ++ show protocolVersion ++ "},\"players\":{\"max\": 100,\"online\": " ++ show (length playersOnline) ++ ",\"sample\":[" ++ userSample ++ "]},\"description\":{\"text\":\"An Experimental Minecraft server written in Haskell | github.com/Lazersmoke/civskell\"},\"favicon\":\"" ++ image ++ "\"}"
    -- Send resp to clients
    sendPacket (Client.StatusResponse resp)
    -- TODO
    -- setPlayerState WaitingForStatusPing
  parsePacket = try (specificVarInt 0x00 >> endOfInput) >> return StatusRequest

data StatusPing = StatusPing Int64
instance Packet StatusPing where
  type PacketSide StatusPing = 'Server
  type PacketState StatusPing = 'Status
  packetName = "StatusPing"
  packetId = 0x01
  packetPretty (StatusPing i) = [("Ping Token","0x" ++ showHex i "")]
  -- Send a pong with the same ping token right away
  onPacket (StatusPing l) = sendPacket (Client.StatusPong l)
  parsePacket = do
    specificVarInt 0x01
    StatusPing <$> parseLong

-- TODO: make this not cancer
image :: String
image = "data:image/png;base64,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"
