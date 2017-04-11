module Main where

import Civskell

-- This is user code
main :: IO ()
main = do
  putStrLn "Starting..."
  runServer defaultConfiguration {shouldLog = \l -> case l of {NormalLog -> True; TaggedLog _ -> True; _ -> False}}
