{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Civskell.Data.Logging where

import Control.Eff
import Civskell.Data.Types

type HasLogging = Member Logging

data Logging a where
  LogString :: LogLevel -> String -> Logging ()

data LogLevel = HexDump | ClientboundPacket | ServerboundPacket | ErrorLog | VerboseLog | NormalLog deriving Eq

logg :: HasLogging r => String -> Eff r ()
logg = send . LogString NormalLog

logLevel :: HasLogging r => LogLevel -> String -> Eff r ()
logLevel l s = send (LogString l s)

runLogger :: HasIO r => Eff (Logging ': r) a -> Eff r a
runLogger (Pure x) = return x
runLogger (Eff u q) = case u of
  Inject (LogString level str) -> case level of
    HexDump -> do
      --liftIO (putStrLn str)
      runLogger (runTCQ q ())
    ClientboundPacket -> do
      liftIO (putStrLn ("[\x1b[32mSent\x1b[0m] " ++ str))
      runLogger (runTCQ q ())
    ServerboundPacket -> do
      liftIO (putStrLn ("[\x1b[32mGot\x1b[0m] " ++ str))
      runLogger (runTCQ q ())
    ErrorLog -> do
      liftIO (putStrLn $ "[\x1b[31mERROR\x1b[0m] " ++ str)
      runLogger (runTCQ q ())
    VerboseLog -> do
      --liftIO (putStrLn $ "[\x1b[36mCivSkell/Verbose\x1b[0m] " ++ str)
      runLogger (runTCQ q ())
    NormalLog -> do
      liftIO (putStrLn $ "[\x1b[36mCivSkell\x1b[0m] " ++ str)
      runLogger (runTCQ q ())
  Weaken otherEffects -> Eff otherEffects (Singleton (\x -> runLogger (runTCQ q x)))

