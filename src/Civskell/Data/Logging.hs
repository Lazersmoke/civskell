{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Logging where

import Control.Monad.Freer
import Control.Monad.Freer.Writer
import Control.Concurrent.STM
import Data.Text (Text)

-- Logging to console without overwriting all the other threads that are also logging to console is an effect
type Logging = Writer LogMessage

-- Log a string at a given Log Level
data LogMessage = LogMessage LogLevel Text

-- TODO: replace this with a `data LogSpec = LogSpec {spec :: String -> String,level :: Int}`?
-- Level of verbosity to log at
data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | VerboseLog | TaggedLog Text | NormalLog deriving Eq

-- A thing that shuffles log messages off of running threads and logs that whenever on a dedicated one.
newtype LogQueue = LogQueue (TQueue Text)

-- Make a new, empty LogQueue
freshLogQueue :: Member IO r => Eff r LogQueue
freshLogQueue = LogQueue <$> send newTQueueIO 

-- Synonym for logging with default precedence
{-# INLINE logg #-}
logg :: Member Logging r => Text -> Eff r ()
logg = logLevel NormalLog

-- Synonym for logging an error
{-# INLINE loge #-}
loge :: Member Logging r => Text -> Eff r ()
loge = logLevel ErrorLog

-- Synonym for logging something with a special tag
{-# INLINE logt #-}
logt :: Member Logging r => Text -> Text -> Eff r ()
logt tag = logLevel (TaggedLog tag)

-- Synonym for Logging with a specified log level
{-# INLINE logLevel #-}
logLevel :: Member Logging r => LogLevel -> Text -> Eff r ()
logLevel l s = tell (LogMessage l s)

--forkLogger :: (Configured r,Logs q,PerformsIO r) => Eff (Logging ': r) a -> Eff q (Eff r a)
--forkLogger = send . ForkLogger

