module Main where

import Civskell

-- This is user code
main :: IO ()
main = do
  putStrLn "Starting..."
  runServer defaultConfiguration {shouldLog = const True} -- \l -> case l of {NormalLog -> True; TaggedLog _ -> True; _ -> False}}
