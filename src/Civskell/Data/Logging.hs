{-# LANGUAGE TypeOperators #-}
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

import Civskell.Data.Types

{-# INLINE logg #-}
logg :: Logs r => String -> Eff r ()
logg = send . LogString NormalLog

{-# INLINE loge #-}
loge :: Logs r => String -> Eff r ()
loge = send . LogString ErrorLog

{-# INLINE logt #-}
logt :: Logs r => String -> String -> Eff r ()
logt tag msg = send $ LogString (TaggedLog tag) msg

{-# INLINE logLevel #-}
logLevel :: Logs r => LogLevel -> String -> Eff r ()
logLevel l s = send (LogString l s)

forkLogger :: (Configured r,Logs q,PerformsIO r) => Eff (Logging ': r) a -> Eff q (Eff r a)
forkLogger = send . ForkLogger

runLogger :: (Configured r,PerformsIO r) => TQueue String -> Eff (Logging ': r) a -> Eff r a
runLogger _ (Pure x) = return x
runLogger s (Eff u q) = case u of
  Inject (ForkLogger e) -> runLogger s (runTCQ q (runLogger s e))
  Inject (LogString level str) -> do
    -- Apply the configured loggin predicate to see if this message should be logged
    p <- ($ level) . shouldLog <$> ask 
    -- Select the prefix, then send off the log message
    when p $ send . atomically . writeTQueue s . (++str) $ case level of
      HexDump -> ""
      ClientboundPacket -> "[\x1b[32mSent\x1b[0m] "
      ServerboundPacket -> "[\x1b[32mRecv\x1b[0m] "
      ErrorLog -> "[\x1b[31m\x1b[1mError\x1b[0m] "
      VerboseLog -> "[\x1b[36mCivSkell/Verbose\x1b[0m] "
      (TaggedLog tag) -> "[\x1b[36m" ++ tag ++ "\x1b[0m] "
      NormalLog -> "[\x1b[36mCivSkell\x1b[0m] "
    -- Return regardless of log level
    runLogger s (runTCQ q ())
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runLogger s (runTCQ q x)))
