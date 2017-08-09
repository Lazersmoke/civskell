{-# LANGUAGE OverloadedStrings #-}
module Main where

import Civskell
import Civskell.Versions.Vanilla (vanilla1_12_1)

-- This is user code
main :: IO ()
main = do
  putStrLn "Starting..."
  runServer vanilla1_12_1 --{shouldLog = const True}
