{-# LANGUAGE OverloadedStrings #-}
module Main where

import Civskell
import Civskell.Versions.Protocol

-- This is user code
main :: IO ()
main = do
  putStrLn "Starting..."
  runServer protocol340 --{shouldLog = const True}
