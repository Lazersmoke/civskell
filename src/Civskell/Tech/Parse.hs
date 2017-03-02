{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Civskell.Tech.Parse where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import Text.Parsec
import Text.Parsec.ByteString
import Unsafe.Coerce
import qualified Data.ByteString as BS
import Data.NBT
import qualified Data.Serialize as Ser

import Civskell.Data.Types
import qualified Civskell.Packet.Serverbound as Server

--unwrapPacket :: ServerPacket -> (forall p. Packet p => p)
--unwrapPacket (ServerPacket p) = p

anyByte :: Parser Word8
anyByte = fromIntegral . ord <$> anyChar

parseHandshakePacket :: Parser (ServerPacket 'Handshaking) --(Packet p, PacketSide p ~ 'Server, PacketState p ~ 'Handshaking) => Parser p
parseHandshakePacket = choice [ServerPacket <$> parseHandshake, ServerPacket <$> parseLegacyHandshake] <?> "Handshake Packet"

parseHandshake :: Parser Server.Handshake
parseHandshake = do
  specificVarInt 0x00
  Server.Handshake
    <$> parseVarInt
    <*> parseVarString
    <*> parseUnsignedShort
    <*> parseVarInt

parseLegacyHandshake :: Parser Server.LegacyHandshake
parseLegacyHandshake = do
  _ <- specificByte 0xFE
  _ <- many anyByte <* eof
  return Server.LegacyHandshake

parseLoginPacket :: Parser (ServerPacket 'LoggingIn)
parseLoginPacket = choice [ServerPacket <$> parseLoginStart, ServerPacket <$> parseEncryptionResponse] <?> "Login Packet"

parseLoginStart :: Parser Server.LoginStart
parseLoginStart = do
  specificVarInt 0x00
  Server.LoginStart <$> parseVarString

parseEncryptionResponse :: Parser Server.EncryptionResponse
parseEncryptionResponse = do
  specificVarInt 0x01
  ssLen <- fromEnum <$> parseVarInt
  ss <- BS.pack <$> replicateM ssLen anyByte
  vtLen <- fromEnum <$> parseVarInt
  vt <- BS.pack <$> replicateM vtLen anyByte
  return $ Server.EncryptionResponse ss vt

parseStatusPacket :: Parser (ServerPacket 'Status)
parseStatusPacket = choice [ServerPacket <$> parseStatusRequest, ServerPacket <$> parseStatusPing] <?> "Status Packet"

parseStatusRequest :: Parser Server.StatusRequest
parseStatusRequest = try (specificVarInt 0x00 >> eof) >> return Server.StatusRequest

parseStatusPing :: Parser Server.StatusPing
parseStatusPing = do
  specificVarInt 0x01
  Server.StatusPing <$> parseLong


parsePlayPacket :: Parser (ServerPacket 'Playing)
parsePlayPacket = choice 
  [ServerPacket <$> parseTPConfirm
  ,ServerPacket <$> parseChatMessage
  ,ServerPacket <$> parseClientStatus
  ,ServerPacket <$> parseClientSettings
  ,ServerPacket <$> parseConfirmTransaction
  ,ServerPacket <$> parseClickWindow
  ,ServerPacket <$> parseCloseWindow
  ,ServerPacket <$> parsePluginMessage
  ,ServerPacket <$> parseKeepAlive
  ,ServerPacket <$> parsePlayerPosition
  ,ServerPacket <$> parsePlayerPositionAndLook
  ,ServerPacket <$> parsePlayerLook
  ,ServerPacket <$> parsePlayer
  ,ServerPacket <$> parsePlayerDigging
  ,ServerPacket <$> parseEntityAction
  ,ServerPacket <$> parseHeldItemChange
  ,ServerPacket <$> parseCreativeInventoryAction
  ,ServerPacket <$> parseAnimation
  ,ServerPacket <$> parsePlayerBlockPlacement
  ,ServerPacket <$> parseUseItem
  ,parseUnknownPacket] <?> "Play Packet"

parseUnknownPacket :: Parser (ServerPacket s)
parseUnknownPacket = anyByte >>= unexpected . show

parseTPConfirm :: Parser Server.TPConfirm
parseTPConfirm = do
  specificVarInt 0x00 <?> "Packet Id 0x00"
  Server.TPConfirm <$> parseVarInt

parseChatMessage :: Parser Server.ChatMessage
parseChatMessage = do
  specificVarInt 0x02 <?> "Packet Id 0x02"
  Server.ChatMessage <$> parseVarString

parseClientStatus :: Parser Server.ClientStatus
parseClientStatus = do
  specificVarInt 0x03 <?> "Packet Id 0x03"
  Server.ClientStatus <$> choice
    [specificVarInt 0x00 *> pure PerformRespawn
    ,specificVarInt 0x01 *> pure RequestStats
    ,specificVarInt 0x02 *> pure OpenInventory
    ]

parseClientSettings :: Parser Server.ClientSettings
parseClientSettings = do
  specificVarInt 0x04 <?> "Packet Id 0x04"
  Server.ClientSettings
    <$> parseVarString
    <*> anyByte
    <*> parseVarInt
    <*> parseBool
    <*> anyByte
    <*> parseVarInt

parseConfirmTransaction :: Parser Server.ConfirmTransaction
parseConfirmTransaction = do
  specificVarInt 0x05 <?> "Packet Id 0x05"
  Server.ConfirmTransaction
    <$> parseWID
    <*> parseShort
    <*> parseBool

parseClickWindow :: Parser Server.ClickWindow
parseClickWindow = do
  specificVarInt 0x07 <?> "Packet Id 0x07"
  wid <- parseWID
  slotNum <- parseShort
  b <- anyByte
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
  return $ Server.ClickWindow wid slotNum transId mode sl

parseCloseWindow :: Parser Server.CloseWindow
parseCloseWindow = do
  specificVarInt 0x08 <?> "Packet Id 0x08"
  Server.CloseWindow <$> parseWID

parsePluginMessage :: Parser Server.PluginMessage
parsePluginMessage = do
  specificVarInt 0x09 <?> "Packet Id 0x09"
  Server.PluginMessage
    <$> parseVarString
    <*> (BS.pack <$> many anyByte) <* eof

parseKeepAlive :: Parser Server.KeepAlive
parseKeepAlive  = do
  specificVarInt 0x0B <?> "Packet Id 0x0B"
  Server.KeepAlive <$> parseVarInt

parsePlayerPosition :: Parser Server.PlayerPosition
parsePlayerPosition = do
  specificVarInt 0x0C <?> "Packet Id 0x0C"
  Server.PlayerPosition
    <$> ((,,) <$> parseDouble <*> parseDouble <*> parseDouble)
    <*> parseBool

parsePlayerPositionAndLook :: Parser Server.PlayerPositionAndLook
parsePlayerPositionAndLook = do
  specificVarInt 0x0D <?> "Packet Id 0x0D"
  Server.PlayerPositionAndLook
    <$> ((,,) <$> parseDouble <*> parseDouble <*> parseDouble)
    <*> ((,) <$> parseFloat <*> parseFloat)
    <*> parseBool

parsePlayerLook :: Parser Server.PlayerLook
parsePlayerLook = do
  specificVarInt 0x0E <?> "Packet Id 0x0E"
  Server.PlayerLook
    <$> ((,) <$> parseFloat <*> parseFloat)
    <*> parseBool

parsePlayer :: Parser Server.Player
parsePlayer = do
  specificVarInt 0x0F <?> "Packet Id 0x0F"
  Server.Player <$> parseBool

parsePlayerDigging :: Parser Server.PlayerDigging
parsePlayerDigging = do
  specificVarInt 0x13 <?> "Packet Id 0x13"
  Server.PlayerDigging <$> choice
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

parseEntityAction :: Parser Server.EntityAction
parseEntityAction = do
  specificVarInt 0x14 <?> "Packet Id 0x14"
  Server.EntityAction <$> parseEID <*> choice
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

parseHeldItemChange :: Parser Server.HeldItemChange
parseHeldItemChange = do
  specificVarInt 0x17 <?> "Packet Id 0x17"
  Server.HeldItemChange <$> parseShort

parseCreativeInventoryAction :: Parser Server.CreativeInventoryAction
parseCreativeInventoryAction = do
  specificVarInt 0x18 <?> "Packet Id 0x18"
  Server.CreativeInventoryAction <$> parseShort <*> parseSlot

parseAnimation :: Parser Server.Animation
parseAnimation = do
  specificVarInt 0x1A <?> "Packet Id 0x1A"
  Server.Animation <$> parseHand

parsePlayerBlockPlacement :: Parser Server.PlayerBlockPlacement
parsePlayerBlockPlacement = do
  specificVarInt 0x1C <?> "Packet Id 0x1C"
  Server.PlayerBlockPlacement
    <$> parseBlockCoord
    <*> parseBlockFace
    <*> parseHand
    <*> ((,,) <$> parseFloat <*> parseFloat <*> parseFloat)

parseUseItem :: Parser Server.UseItem
parseUseItem = do
  specificVarInt 0x1D <?> "Packet Id 0x1D"
  Server.UseItem <$> parseHand

parseHand :: Parser Hand
parseHand = (specificVarInt 0x01 *> pure OffHand) <|> (specificVarInt 0x00 *> pure MainHand)

parseBlockFace :: Parser BlockFace
parseBlockFace = choice
  [specificByte 0x00 *> pure Bottom
  ,specificByte 0x01 *> pure Top
  ,specificByte 0x02 *> pure North
  ,specificByte 0x03 *> pure South
  ,specificByte 0x04 *> pure West
  ,specificByte 0x05 *> pure East
  ]

parseVarString :: Parser String
parseVarString = do
  size <- fromEnum <$> parseVarInt
  replicateM size anyChar

parseSlot :: Parser Slot
parseSlot = try emptySlot <|> nonEmptySlot
  where
    emptySlot = do
      bid <- parseShort
      guard $ bid == -1
      return EmptySlot
    nonEmptySlot = do
      bid <- parseShort
      cnt <- anyByte
      dmg <- parseShort
      nbtFlair <- anyByte
      case nbtFlair of
        0 -> return $ Slot bid cnt dmg Nothing
        n -> do
          ns <- BS.pack <$> many anyByte
          eof
          return $ Slot bid cnt dmg (Just . (\(Right (NBT _ a)) -> a) $ Ser.decode (n `BS.cons` ns))

parseBlockCoord :: Parser (Block r)
parseBlockCoord = do
  xyz <- parseLong
  let x = u (shiftR xyz 38)
  let x' = if x >= 2^(25 :: Integer) then x - 2^(26 :: Integer) else x
  let y = u $ (shiftR xyz 26) .&. 0xFFF
  let y' = if y >= 2^(11 :: Integer) then y - 2^(12 :: Integer) else y
  let z = u $ xyz .&. 0x3FFFFFF
  let z' = if z >= 2^(25 :: Integer) then z - 2^(26 :: Integer) else z
  return $ Block (x',y',z')
  where
    u = unsafeCoerce :: Int64 -> Int

parseShort :: Parser Int16
parseShort = do
  firstByte <- (unsafeCoerce :: Word8 -> Int16) <$> anyByte
  secondByte <- (unsafeCoerce :: Word8 -> Int16) <$> anyByte
  return $ (shiftL firstByte 8) .|. secondByte

parseUnsignedShort :: Parser Word16
parseUnsignedShort = do
  firstByte <- (unsafeCoerce :: Word8 -> Word16) <$> anyByte
  secondByte <- (unsafeCoerce :: Word8 -> Word16) <$> anyByte
  return $ (shiftL firstByte 8) .|. secondByte

parseVarInt :: Parser VarInt
parseVarInt = do
  b <- anyByte
  let thisPart = (unsafeCoerce :: Word8 -> VarInt) (clearBit b 7)
  if testBit b 7
    then do
      nb <- parseVarInt
      return $ thisPart .|. (shiftL nb 7)
    else return thisPart

parseWID :: Parser WindowId
parseWID = WindowId <$> anyByte

parseEID :: Parser EntityId
parseEID = EntityId <$> parseVarInt

specificVarInt :: VarInt -> Parser ()
specificVarInt v = try $ do
  v' <- parseVarInt
  guard (v == v') <?> ("VarInt " ++ show v)

specificByte :: Word8 -> Parser ()
specificByte v = try $ do
  v' <- anyByte
  guard (v == v') <?> ("Byte " ++ show v)

parseBool :: Parser Bool
parseBool = (specificVarInt 0x00 *> pure False) <|> (specificVarInt 0x01 *> pure True)

parseLong :: Parser Int64
parseLong = do
  bs <- replicateM 8 $ (unsafeCoerce :: Word8 -> Int64) <$> anyByte
  return $ foldl1 (\a x -> shiftL a 8 .|. x) bs

parseDouble :: Parser Double
parseDouble = (unsafeCoerce :: Int64 -> Double) <$> parseLong

parseInt :: Parser Int32
parseInt = do
  bs <- replicateM 4 $ (unsafeCoerce :: Word8 -> Int32) <$> anyByte
  return $ foldl1 (\a x -> shiftL a 8 .|. x) bs

parseFloat :: Parser Float
parseFloat = (unsafeCoerce :: Int32 -> Float) <$> parseInt

parseCompPkt :: Parser (VarInt,BS.ByteString)
parseCompPkt = do
  _ <- parseVarInt
  dataLen <- parseVarInt
  bs <- BS.pack <$> many anyByte
  eof
  return (dataLen,bs)

parseUncompPkt :: Parser BS.ByteString
parseUncompPkt = do
  pktLen <- parseVarInt
  bs <- BS.pack <$> replicateM (fromIntegral pktLen) anyByte
  eof
  return bs

-- Comes out to -1
parseTest :: IO ()
parseTest = case parse parseShort "" (BS.pack [0xff,0xff]) of
  Right a -> print a
  Left e -> print e

--satisfyBS :: (Word8 -> Bool) -> BSParser Word8
--satisfyBS p = tokenPrim (show . chr . fromEnum) nextpos boolToMaybe
  --where
    --nextpos prev thisTok otherToks = incSourceColumn prev 1
    --boolToMaybe x = if p x then Just x else Nothing
