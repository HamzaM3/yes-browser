{-# LANGUAGE TypeOperators #-}

module ParserUtils.ParsingUtils where

import Control.Lens.Lens ((??))
import Data.Char (isSpace)
import GHC.Base (ord)
import Graphics.GL (GLfloat)
import Graphics.Rendering.OpenGL (Color4 (Color4))

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

groupSpaces :: String -> String
groupSpaces s = group $ span isSpace s
  where
    group :: (String, String) -> String
    group (_, "") = ""
    group ("", s) = a ++ group (span isSpace b)
      where
        (a, b) = break isSpace s
    group (_, s) = " " ++ group ("", s)

isNotForbidden :: [Char] -> (Char -> Bool)
isNotForbidden forbidden = and . (map (/=) forbidden ??)

isAuthorized :: [Char] -> (Char -> Bool)
isAuthorized authorized = or . (map (==) authorized ??)

hexToColor4 :: String -> Color4 GLfloat
hexToColor4 = tripletToColor . hexToTriplet . map hexDigitToNum
  where
    tripletToColor :: [GLfloat] -> Color4 GLfloat
    tripletToColor [a, b, c] = Color4 a b c 1.0
    hexToTriplet :: [Int] -> [GLfloat]
    hexToTriplet [] = []
    hexToTriplet (a : b : rest) = hexPairToNum a b : hexToTriplet rest
      where
        hexPairToNum :: Int -> Int -> GLfloat
        hexPairToNum a b = fromIntegral (16 * a + b) / 255.0
    hexDigitToNum :: Char -> Int
    hexDigitToNum c
      | ord c >= ord '0' && ord c <= ord '9' = ord c - ord '0'
      | ord c >= ord 'A' && ord c <= ord 'F' = ord c - ord 'A' + 10
      | ord c >= ord 'a' && ord c <= ord 'f' = ord c - ord 'a' + 10
