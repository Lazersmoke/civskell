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
import Control.Concurrent.MVar

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

runLogger :: HasIO r => MVar () -> Eff (Logging ': r) a -> Eff r a
runLogger _ (Pure x) = return x
runLogger s (Eff u q) = case u of
  Inject (LogString level str) -> case level of
    HexDump -> do
      --withLock s (putStrLn str)
      runLogger s (runTCQ q ())
    ClientboundPacket -> do
      withLock s (putStrLn ("[\x1b[32mSent\x1b[0m] " ++ str))
      runLogger s (runTCQ q ())
    ServerboundPacket -> do
      withLock s (putStrLn ("[\x1b[32mRecv\x1b[0m] " ++ str))
      runLogger s (runTCQ q ())
    ErrorLog -> do
      withLock s (putStrLn $ "[\x1b[31m\x1b[1mError\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
    VerboseLog -> do
      withLock s (putStrLn $ "[\x1b[36mCivSkell/Verbose\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
    (TaggedLog tag) -> do
      withLock s (putStrLn $ "[\x1b[36m" ++ tag ++ "\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
    NormalLog -> do
      withLock s (putStrLn $ "[\x1b[36mCivSkell\x1b[0m] " ++ str)
      runLogger s (runTCQ q ())
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runLogger s (runTCQ q x)))

withLock :: HasIO r => MVar () -> IO a -> Eff r a
withLock lock ma = send (withMVar lock $ const ma)
