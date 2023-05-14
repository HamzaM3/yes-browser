module ParserUtils.CheckParser where

import Control.Applicative
import ParserUtils.Parser

checkLength :: Int -> String -> Parser String
checkLength l s = if length s == l then pure s else empty