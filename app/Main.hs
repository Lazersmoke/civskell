module Main where

import Civskell

-- This is user code
main :: IO ()
main = do
  putStrLn "Starting..."
  runServer defaultConfiguration
