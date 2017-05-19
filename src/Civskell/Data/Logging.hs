{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Logging 
  (logg,loge,logt
  ,logLevel
  ,LogLevel(..)
  ,forkLogger
  ,runLogger
  ) where

import Control.Eff
import Control.Eff.Reader
import Control.Monad (when)
import Control.Concurrent.STM
import Data.Text (Text)
import Data.Semigroup ((<>))

import Civskell.Data.Types

-- Synonym for logging with default precedence
{-# INLINE logg #-}
logg :: Logs r => Text -> Eff r ()
logg = send . LogText NormalLog

-- Synonym for logging an error
{-# INLINE loge #-}
loge :: Logs r => Text -> Eff r ()
loge = send . LogText ErrorLog

-- Synonym for logging something with a special tag
{-# INLINE logt #-}
logt :: Logs r => Text -> Text -> Eff r ()
logt tag msg = send $ LogText (TaggedLog tag) msg

-- Synonym for Logging with a specified log level
{-# INLINE logLevel #-}
logLevel :: Logs r => LogLevel -> Text -> Eff r ()
logLevel l s = send (LogText l s)

forkLogger :: (Configured r,Logs q,PerformsIO r) => Eff (Logging ': r) a -> Eff q (Eff r a)
forkLogger = send . ForkLogger

runLogger :: (Configured r,PerformsIO r) => LogQueue -> Eff (Logging ': r) a -> Eff r a
runLogger _ (Pure x) = return x
runLogger s@(LogQueue l) (Eff u q) = case u of
  Inject (ForkLogger e) -> runLogger s (runTCQ q (runLogger s e))
  Inject (LogText level str) -> do
    -- Apply the configured loggin predicate to see if this message should be logged
    p <- ($ level) . shouldLog <$> ask 
    -- Select the prefix, then send off the log message
    when p $ send . atomically . writeTQueue l . (<>str) $ case level of
      HexDump -> ""
      ClientboundPacket -> "[\x1b[32mSent\x1b[0m] "
      ServerboundPacket -> "[\x1b[32mRecv\x1b[0m] "
      ErrorLog -> "[\x1b[31m\x1b[1mError\x1b[0m] "
      VerboseLog -> "[\x1b[36mCivSkell/Verbose\x1b[0m] "
      (TaggedLog tag) -> "[\x1b[36m" <> tag <> "\x1b[0m] "
      NormalLog -> "[\x1b[36mCivSkell\x1b[0m] "
    -- Return regardless of log level
    runLogger s (runTCQ q ())
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runLogger s (runTCQ q x)))
