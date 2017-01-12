module Main where

import Lib
import OpenSSL

main :: IO ()
main = withOpenSSL $ do
  putStrLn "Starting..."
  startListening
