module Main where

import Piskell

-- This is user code
main :: IO ()
main = do
  putStrLn "Starting..."
  runServer defaultConfiguration {shouldLog = \l -> case l of {NormalLog -> True; TaggedLog _ -> True; _ -> False}}
