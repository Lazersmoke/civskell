{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Civskell.Data.Util where

import qualified Data.Text as T

import Civskell.Data.Types

{- Redesign of the Civskell module hierarchy:
 - Some code manipulates the World state.
 - Some code works on a particular player, purely.
 -
 - We need a special effect interpreter to manage interactions, but these can both be `State s` for some `s`
 - We *don't* need a special effect ADT for each of these effects.
 -}

showText :: Show a => a -> T.Text
showText = T.pack . show

-- Simple way to inject a text message into a json chat string. No additional
-- formatting or checking is done, just raw text.
jsonyText :: String -> ProtocolString
jsonyText s = ProtocolString $ "{\"text\":\"" ++ s ++ "\"}"

