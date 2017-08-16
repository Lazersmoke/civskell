module Civskell.Data.Util where

import qualified Data.Text as T

import Civskell.Data.Types

showText :: Show a => a -> T.Text
showText = T.pack . show

-- Simple way to inject a text message into a json chat string. No additional
-- formatting or checking is done, just raw text.
jsonyText :: String -> ProtocolString
jsonyText s = ProtocolString $ "{\"text\":\"" ++ s ++ "\"}"

