{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module ParseBS where

import Text.Parsec.ByteString
import Text.Parsec
import qualified Data.ByteString as BS
import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import Control.Monad
import Unsafe.Coerce

import qualified Serverbound as Server

import Data


anyByte :: Parser Word8
anyByte = fromIntegral . ord <$> anyChar

parsePacket :: Parser Server.Packet
parsePacket = try parseHandshakePacket <|> try parseLoginPacket <|> parseStatusPacket

parseHandshakePacket :: Parser Server.Packet
parseHandshakePacket = parseHandshake

parseLoginPacket :: Parser Server.Packet
parseLoginPacket = parseLoginStart

parseStatusPacket :: Parser Server.Packet
parseStatusPacket = parseStatusRequest <|> parseStatusPing

parseStatusRequest :: Parser Server.Packet
parseStatusRequest = try (specificVarInt 0x00 >> eof) >> return Server.StatusRequest

parseStatusPing :: Parser Server.Packet
parseStatusPing = do
  try (specificVarInt 0x01) 
  Server.StatusPing <$> parseLong 

parseHandshake :: Parser Server.Packet
parseHandshake = do
  try $ specificVarInt 0x00
  Server.Handshake 
    <$> parseVarInt 
    <*> parseVarString 
    <*> parseUnsignedShort
    <*> parseVarInt

parseLoginStart :: Parser Server.Packet
parseLoginStart = do
  try $ specificVarInt 0x00
  Server.LoginStart <$> parseVarString

parseVarString :: Parser String
parseVarString = do
  size <- fromEnum <$> parseVarInt
  replicateM size anyChar

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

parseLong :: Parser Int64
parseLong = do
  bs <- replicateM 8 $ (unsafeCoerce :: Word8 -> Int64) <$> anyByte
  return $ foldl1 (\a x -> shiftL a 8 .|. x) bs

-- Comes out to -1
parseTest :: IO ()
parseTest = case parse ((const 5 <$> parseHandshake) <|> parseVarInt) "" (BS.pack [0xff,0xff,0xff,0xff,0x0f]) of
  Right a -> print a
  Left e -> print e

--satisfyBS :: (Word8 -> Bool) -> BSParser Word8
--satisfyBS p = tokenPrim (show . chr . fromEnum) nextpos boolToMaybe
  --where
    --nextpos prev thisTok otherToks = incSourceColumn prev 1
    --boolToMaybe x = if p x then Just x else Nothing
