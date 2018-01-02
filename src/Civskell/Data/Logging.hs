{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Logging where

import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup
import Control.Monad
import Control.Monad.Reader
import Control.Lens

import Civskell.Data.Types

-- Make a new, empty LogQueue
freshLogQueue :: IO LogQueue
freshLogQueue = LogQueue <$> newTQueueIO 

-- Synonym for logging with default precedence
{-# INLINE logg #-}
logg :: Text -> Civskell ()
logg = logLevel NormalLog

-- Synonym for logging an error
{-# INLINE loge #-}
loge :: Text -> Civskell ()
loge = logLevel ErrorLog

-- Synonym for logging something with a special tag
{-# INLINE logt #-}
logt :: Text -> Text -> Civskell ()
logt tag = logLevel (TaggedLog tag)

logp :: Text -> Civskell ()
logp msg = do
  pla <- asks playerData
  flip logt msg =<< (T.pack . view playerUsername <$> lift (readTVarIO pla))

-- Logging with a specified log level, based on configured logging level
logLevel :: LogLevel -> Text -> Civskell ()
logLevel level str = do
  c <- asks configuration
  l <- asks globalLogQueue
  lift $ logToQueue c l level str

-- Log the given message at the given log level, using the given configuration
-- Apply the configured logging predicate to see if this message should be logged
logToQueue :: Configuration -> LogQueue -> LogLevel -> Text -> IO ()
logToQueue c (LogQueue l) level str = when (shouldLog c level) . atomically . writeTQueue l . (<>str) $ case level of
  HexDump -> ""
  ClientboundPacket -> "[\x1b[32mSent\x1b[0m] "
  ServerboundPacket -> "[\x1b[32mRecv\x1b[0m] "
  ErrorLog -> "[\x1b[31m\x1b[1mError\x1b[0m] "
  VerboseLog -> "[\x1b[36m" <> serverName c <> "/Verbose\x1b[0m] "
  (TaggedLog tag) -> "[\x1b[36m" <> tag <> "\x1b[0m] "
  NormalLog -> "[\x1b[36m" <> serverName c <> "\x1b[0m] "
