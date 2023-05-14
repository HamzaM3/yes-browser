{-# LANGUAGE TypeOperators #-}

module ParserUtils.ParsingUtils where

import Data.Char (isSpace)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> c) -> (b, a) -> (b, c)
mapSnd f (a, b) = (a, f b)

mapTuple :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapTuple f g (x, y) = (f x, g y)

type a :+: b = Either a b

enum :: (Enum a, Bounded a) => [a]
enum = enumFrom (toEnum 0)

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf f x = if f x then Nothing else Just x

reverseSpan :: (a -> Bool) -> [a] -> ([a], [a])
reverseSpan cd = mapTuple reverse reverse . span cd . reverse

removeTailWhiteSpace :: String -> String
removeTailWhiteSpace = snd . reverseSpan isSpace