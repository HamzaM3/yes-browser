module Parsers.ParserUtils.CheckParser where

import Control.Applicative
import Parsers.ParserUtils.Parser

{-
  Dependent parsing
  meant to be combined with (>>=)
    -> check the Monad definition of Parser
  TODO:
  - improve along with the Parser
-}

checkLength :: Int -> String -> Parser String
checkLength l s = if length s == l then pure s else empty

checkString :: String -> String -> Parser String
checkString l s = if s == l then pure s else empty
