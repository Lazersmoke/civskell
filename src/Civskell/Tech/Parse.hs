{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Civskell.Tech.Parse where

import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Bytes.Get
import Data.Bytes.Serial
import Data.Char
import Data.Int
import Data.Word
import Data.Attoparsec.ByteString
import Unsafe.Coerce
import qualified Data.ByteString as BS

import Civskell.Data.Types
import Civskell.Data.Protocol


anyChar :: Parser Char
anyChar = chr . fromIntegral <$> anyWord8

parseHand :: Parser Hand
parseHand = (specificVarInt 0x01 *> pure OffHand) <|> (specificVarInt 0x00 *> pure MainHand)

parseUncompPkt :: MonadGet m => m BS.ByteString
parseUncompPkt = getByteString . fromIntegral =<< deserialize @VarInt

parseBlockFace :: Parser BlockFace
parseBlockFace = toEnum . unsafeCoerce <$> anyWord8
  --[specificByte 0x00 *> pure Bottom
  --,specificByte 0x01 *> pure Top
  --,specificByte 0x02 *> pure North
  --,specificByte 0x03 *> pure South
  --,specificByte 0x04 *> pure West
  --,specificByte 0x05 *> pure East
  --]

parseVarString :: Parser String
parseVarString = do
  size <- fromEnum <$> parseVarInt
  replicateM size anyChar

{-
parseSlot :: Parser Slot
parseSlot = try emptySlot <|> nonEmptySlot
  where
    emptySlot = do
      bid <- parseShort
      guard $ bid == -1
      return EmptySlot
    nonEmptySlot = do
      bid <- parseShort
      cnt <- anyWord8
      dmg <- parseShort
      nbtFlair <- anyWord8
      case nbtFlair of
        0 -> return $ Slot bid cnt dmg Nothing
        n -> do
          ns <- BS.pack <$> many anyWord8
          endOfInput
          return $ Slot bid cnt dmg (Just . (\(Right (NBT _ a)) -> a) $ Ser.decode (n `BS.cons` ns))
-}

parseBlockCoord :: Parser (BlockLocation r)
parseBlockCoord = do
  xyz <- parseLong
  let x = u (shiftR xyz 38)
  let x' = if x >= 2^(25 :: Integer) then x - 2^(26 :: Integer) else x
  let y = u $ (shiftR xyz 26) .&. 0xFFF
  let y' = if y >= 2^(11 :: Integer) then y - 2^(12 :: Integer) else y
  let z = u $ xyz .&. 0x3FFFFFF
  let z' = if z >= 2^(25 :: Integer) then z - 2^(26 :: Integer) else z
  return $ BlockLocation (x',y',z')
  where
    u = unsafeCoerce :: Int64 -> Int

parseShort :: Parser Int16
parseShort = do
  firstByte <- (unsafeCoerce :: Word8 -> Int16) <$> anyWord8
  secondByte <- (unsafeCoerce :: Word8 -> Int16) <$> anyWord8
  return $ (shiftL firstByte 8) .|. secondByte

parseUnsignedShort :: Parser Word16
parseUnsignedShort = do
  firstByte <- (unsafeCoerce :: Word8 -> Word16) <$> anyWord8
  secondByte <- (unsafeCoerce :: Word8 -> Word16) <$> anyWord8
  return $ (shiftL firstByte 8) .|. secondByte

parseVarInt :: Parser VarInt
parseVarInt = do
  b <- anyWord8
  let thisPart = (unsafeCoerce :: Word8 -> VarInt) (clearBit b 7)
  if testBit b 7
    then do
      nb <- parseVarInt
      return $ thisPart .|. (shiftL nb 7)
    else return thisPart

parseWID :: Parser WindowId
parseWID = WindowId <$> anyWord8

parseEID :: Parser EntityId
parseEID = EntityId <$> parseVarInt

specificVarInt :: VarInt -> Parser ()
specificVarInt v = try $ do
  v' <- parseVarInt
  guard (v == v') <?> ("VarInt " ++ show v)

specificByte :: Word8 -> Parser ()
specificByte v = try $ do
  v' <- anyWord8
  guard (v == v') <?> ("Byte " ++ show v)

parseBool :: Parser Bool
parseBool = (specificVarInt 0x00 *> pure False) <|> (specificVarInt 0x01 *> pure True)

parseLong :: Parser Int64
parseLong = do
  bs <- replicateM 8 $ (unsafeCoerce :: Word8 -> Int64) <$> anyWord8
  return $ foldl1 (\a x -> shiftL a 8 .|. x) bs

parseDouble :: Parser Double
parseDouble = (unsafeCoerce :: Int64 -> Double) <$> parseLong

parseInt :: Parser Int32
parseInt = do
  bs <- replicateM 4 $ (unsafeCoerce :: Word8 -> Int32) <$> anyWord8
  return $ foldl1 (\a x -> shiftL a 8 .|. x) bs

parseFloat :: Parser Float
parseFloat = (unsafeCoerce :: Int32 -> Float) <$> parseInt

parseCompPkt :: Parser (VarInt,BS.ByteString)
parseCompPkt = do
  _ <- parseVarInt
  dataLen <- parseVarInt
  bs <- takeByteString
  return (dataLen,bs)
