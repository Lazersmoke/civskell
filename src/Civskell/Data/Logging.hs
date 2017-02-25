{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Logging 
  (logg,logLevel
  ,LogLevel(..)
  ,runLogger
  ,HasLogging
  ) where

import Control.Eff

import Civskell.Data.Types

type HasLogging r = Member Logging r

data Logging a where
  LogString :: LogLevel -> String -> Logging ()

data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | VerboseLog | NormalLog deriving Eq

{-# INLINE logg #-}
logg :: HasLogging r => String -> Eff r ()
logg = send . LogString NormalLog

{-# INLINE logLevel #-}
logLevel :: HasLogging r => LogLevel -> String -> Eff r ()
logLevel l s = send (LogString l s)

runLogger :: HasIO r => Eff (Logging ': r) a -> Eff r a
runLogger (Pure x) = return x
runLogger (Eff u q) = case u of
  Inject (LogString level str) -> case level of
    HexDump -> do
      --send (putStrLn str)
      runLogger (runTCQ q ())
    ClientboundPacket -> do
      send (putStrLn ("[\x1b[32mSent\x1b[0m] " ++ str))
      runLogger (runTCQ q ())
    ServerboundPacket -> do
      send (putStrLn ("[\x1b[32mRecv\x1b[0m] " ++ str))
      runLogger (runTCQ q ())
    ErrorLog -> do
      send (putStrLn $ "[\x1b[31mERROR\x1b[0m] " ++ str)
      runLogger (runTCQ q ())
    VerboseLog -> do
      send (putStrLn $ "[\x1b[36mCivSkell/Verbose\x1b[0m] " ++ str)
      runLogger (runTCQ q ())
    NormalLog -> do
      send (putStrLn $ "[\x1b[36mCivSkell\x1b[0m] " ++ str)
      runLogger (runTCQ q ())
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runLogger (runTCQ q x)))

