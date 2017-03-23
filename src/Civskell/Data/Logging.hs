{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Logging 
  (logg,loge,logt
  ,logLevel
  ,LogLevel(..)
  ,runLogger
  ) where

import Control.Eff
import Control.Concurrent.Chan

import Civskell.Data.Types

{-# INLINE logg #-}
logg :: HasLogging r => String -> Eff r ()
logg = send . LogString NormalLog

{-# INLINE loge #-}
loge :: HasLogging r => String -> Eff r ()
loge = send . LogString ErrorLog

{-# INLINE logt #-}
logt :: HasLogging r => String -> String -> Eff r ()
logt tag msg = send $ LogString (TaggedLog tag) msg

{-# INLINE logLevel #-}
logLevel :: HasLogging r => LogLevel -> String -> Eff r ()
logLevel l s = send (LogString l s)

runLogger :: HasIO r => Chan String -> Eff (Logging ': r) a -> Eff r a
runLogger _ (Pure x) = return x
runLogger s (Eff u q) = case u of
  Inject (LogString level str) -> case level of
    HexDump -> do
      -- Hex dumps disabled for now
      --send $ writeChan s str
      runLogger s (runTCQ q ())
    ClientboundPacket -> do
      send $ writeChan s ("[\x1b[32mSent\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
    ServerboundPacket -> do
      send $ writeChan s ("[\x1b[32mRecv\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
    ErrorLog -> do
      send $ writeChan s ("[\x1b[31m\x1b[1mError\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
    VerboseLog -> do
      send $ writeChan s ("[\x1b[36mCivSkell/Verbose\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
    (TaggedLog tag) -> do
      send $ writeChan s ("[\x1b[36m" ++ tag ++ "\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
    NormalLog -> do
      send $ writeChan s ("[\x1b[36mCivSkell\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runLogger s (runTCQ q x)))
