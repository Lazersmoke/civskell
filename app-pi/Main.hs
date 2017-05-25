{-# LANGUAGE OverloadedStrings #-}
module Main where

import Piskell
import Civskell.Data.Types (defaultConfiguration,shouldLog,serverName)

-- This is user code
main :: IO ()
main = do
  putStrLn "Starting Piskell..."
  runClient defaultConfiguration {shouldLog = const True,serverName = "Piskell"}
