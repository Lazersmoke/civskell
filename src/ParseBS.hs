{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module ParseBS where

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

import Data
import qualified Serverbound as Server

anyByte :: Parser Word8
anyByte = fromIntegral . ord <$> anyChar

parsePacket :: ServerState -> Parser Server.Packet
parsePacket Handshaking = parseHandshakePacket 
parsePacket LoggingIn = parseLoginPacket 
parsePacket Status = parseStatusPacket
parsePacket Playing = parsePlayPacket

parseHandshakePacket :: Parser Server.Packet
parseHandshakePacket = parseHandshake <?> "Handshake Packet"

parseHandshake :: Parser Server.Packet
parseHandshake = do
  try (specificVarInt 0x00)
  Server.Handshake 
    <$> parseVarInt 
    <*> parseVarString 
    <*> parseUnsignedShort
    <*> parseVarInt

parseLoginPacket :: Parser Server.Packet
parseLoginPacket = parseLoginStart <|> parseEncryptionResponse <?> "Login Packet"

parseLoginStart :: Parser Server.Packet
parseLoginStart = do
  try (specificVarInt 0x00)
  Server.LoginStart <$> parseVarString

parseEncryptionResponse :: Parser Server.Packet
parseEncryptionResponse = do
  try (specificVarInt 0x01)
  ssLen <- fromEnum <$> parseVarInt
  ss <- BS.pack <$> replicateM ssLen anyByte
  vtLen <- fromEnum <$> parseVarInt
  vt <- BS.pack <$> replicateM vtLen anyByte
  return $ Server.EncryptionResponse ss vt

parseStatusPacket :: Parser Server.Packet
parseStatusPacket = parseStatusRequest <|> parseStatusPing <?> "Status Packet"

parseStatusRequest :: Parser Server.Packet
parseStatusRequest = try (specificVarInt 0x00 >> eof) >> return Server.StatusRequest

parseStatusPing :: Parser Server.Packet
parseStatusPing = do
  try (specificVarInt 0x01)
  Server.StatusPing <$> parseLong 

parsePlayPacket :: Parser Server.Packet
parsePlayPacket = parseTPConfirm <|> parseChatMessage <|> parseClientStatus <|> parseClientSettings <|> parseCloseWindow <|> parsePluginMessage <|> parseKeepAlive <|> parsePlayerPosition <|> parsePlayerPositionAndLook <|> parsePlayerLook <|> parsePlayer <|> parsePlayerDigging <|> parseEntityAction <|> parseHeldItemChange <|> parseCreativeInventoryAction <|> parseAnimation <|> parsePlayerBlockPlacement <|> parseUseItem <|> parseUnknownPacket <?> "Play Packet"

parseUnknownPacket :: Parser Server.Packet
parseUnknownPacket = anyByte >>= unexpected . show

parseTPConfirm :: Parser Server.Packet
parseTPConfirm = do
  try (specificVarInt 0x00) <?> "Packet Id 0x00"
  Server.TPConfirm <$> parseVarInt

parseChatMessage :: Parser Server.Packet
parseChatMessage = do
  try (specificVarInt 0x02) <?> "Packet Id 0x02"
  Server.ChatMessage <$> parseVarString

parseClientStatus :: Parser Server.Packet
parseClientStatus = do
  try (specificVarInt 0x03) <?> "Packet Id 0x03"
  Server.ClientStatus <$> parseVarInt

parseClientSettings :: Parser Server.Packet
parseClientSettings = do
  try (specificVarInt 0x04) <?> "Packet Id 0x04"
  Server.ClientSettings
    <$> parseVarString
    <*> anyByte
    <*> parseVarInt
    <*> parseBool
    <*> anyByte
    <*> parseVarInt

parseCloseWindow :: Parser Server.Packet
parseCloseWindow = do
  try (specificVarInt 0x08) <?> "Packet Id 0x08"
  Server.CloseWindow <$> anyByte

parsePluginMessage :: Parser Server.Packet
parsePluginMessage = do
  try (specificVarInt 0x09) <?> "Packet Id 0x09"
  Server.PluginMessage
    <$> parseVarString
    <*> (BS.pack <$> many anyByte) <* eof

parseKeepAlive :: Parser Server.Packet
parseKeepAlive  = do
  try (specificVarInt 0x0B) <?> "Packet Id 0x0B"
  Server.KeepAlive <$> parseVarInt

parsePlayerPosition :: Parser Server.Packet
parsePlayerPosition = do
  try (specificVarInt 0x0C) <?> "Packet Id 0x0C"
  Server.PlayerPosition
    <$> ((,,) <$> parseDouble <*> parseDouble <*> parseDouble)
    <*> parseBool

parsePlayerPositionAndLook :: Parser Server.Packet
parsePlayerPositionAndLook = do
  try (specificVarInt 0x0D) <?> "Packet Id 0x0D"
  Server.PlayerPositionAndLook 
    <$> ((,,) <$> parseDouble <*> parseDouble <*> parseDouble)
    <*> ((,) <$> parseFloat <*> parseFloat)
    <*> parseBool

parsePlayerLook :: Parser Server.Packet
parsePlayerLook = do
  try (specificVarInt 0x0E) <?> "Packet Id 0x0E"
  Server.PlayerLook 
    <$> ((,) <$> parseFloat <*> parseFloat)
    <*> parseBool

parsePlayer :: Parser Server.Packet
parsePlayer = do
  try (specificVarInt 0x0F) <?> "Packet Id 0x0F"
  Server.Player <$> parseBool

parsePlayerDigging :: Parser Server.Packet
parsePlayerDigging = do
  try (specificVarInt 0x13) <?> "Packet Id 0x13"
  Server.PlayerDigging
    <$> parseVarInt
    <*> parseBlockCoord
    <*> anyByte

parseEntityAction :: Parser Server.Packet
parseEntityAction = do
  try (specificVarInt 0x14) <?> "Packet Id 0x14"
  Server.EntityAction
    <$> parseVarInt
    <*> parseVarInt
    <*> parseVarInt

parseHeldItemChange :: Parser Server.Packet
parseHeldItemChange = do
  try (specificVarInt 0x17) <?> "Packet Id 0x17"
  Server.HeldItemChange <$> parseShort

parseCreativeInventoryAction :: Parser Server.Packet
parseCreativeInventoryAction = do
  try (specificVarInt 0x18) <?> "Packet Id 0x18"
  Server.CreativeInventoryAction <$> parseShort <*> parseSlot

parseAnimation :: Parser Server.Packet
parseAnimation = do
  try (specificVarInt 0x1A) <?> "Packet Id 0x1A"
  Server.Animation <$> parseVarInt

parsePlayerBlockPlacement :: Parser Server.Packet
parsePlayerBlockPlacement = do
  try (specificVarInt 0x1C) <?> "Packet Id 0x1C"
  Server.PlayerBlockPlacement
    <$> parseBlockCoord
    <*> parseVarInt
    <*> parseVarInt
    <*> ((,,) <$> parseFloat <*> parseFloat <*> parseFloat)

parseUseItem :: Parser Server.Packet
parseUseItem = do
  try (specificVarInt 0x1D) <?> "Packet Id 0x1D"
  Server.UseItem <$> parseVarInt

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

parseBlockCoord :: Parser BlockCoord
parseBlockCoord = do
  xyz <- parseLong
  let x = u (shiftR xyz 38)
  let x' = if x >= 2^(25 :: Integer) then x - 2^(26 :: Integer) else x
  let y = u $ (shiftR xyz 26) .&. 0xFFF
  let y' = if y >= 2^(11 :: Integer) then y - 2^(12 :: Integer) else y
  let z = u $ xyz .&. 0x3FFFFFF
  let z' = if z >= 2^(25 :: Integer) then z - 2^(26 :: Integer) else z
  return $ BlockCoord (x',y',z')
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

specificVarInt :: VarInt -> Parser ()
specificVarInt v = do
  v' <- parseVarInt
  guard (v == v') <?> show v

parseBool :: Parser Bool
parseBool = (try (specificVarInt 0x00) *> pure False) <|> (specificVarInt 0x01 *> pure True)

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
