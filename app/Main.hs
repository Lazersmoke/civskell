{-# LANGUAGE OverloadedStrings #-}
module Main where

import Civskell
import Civskell.Versions.Vanilla (vanilla1_12)

-- This is user code
main :: IO ()
main = do
  putStrLn "Starting..."
  runServer vanilla1_12 --{shouldLog = const True}
