{-# LANGUAGE TupleSections #-}

module ParserUtils.BaseParser where

import Control.Applicative (Alternative (..), (<**>))
import Control.Lens.Lens
import Data.Char (isAlpha, isDigit)
import Data.Tuple (swap)
import ParserUtils.Parser (Parser (..))
import ParserUtils.ParsingUtils (mapSnd, nothingIf)

parseRest :: Parser String
parseRest = p <* (parseChar '\n' <|> pure 'a') -- turn maybe parsers into unfailable parser (not OK)
  where
    p = Parser $ Just . swap . span ('\n' /=)

parseId :: Parser String
parseId = Parser $ Just . (,"")

parseInt :: Parser Int
parseInt = Parser $ fmap (mapSnd read) . nothingIf ((==) "" . snd) . swap . span isDigit

parseChar :: Char -> Parser Char
parseChar c = Parser p
  where
    p (u : rest) = if u == c then Just (rest, u) else Nothing
    p _ = Nothing

parseEnd :: Parser String
parseEnd = Parser $ \input -> nothingIf ((/=) "" . snd) (input, input)

parseAlpha :: Parser String
parseAlpha = Parser $ nothingIf ((==) "" . snd) . swap . span isAlpha

parseDigit :: Parser String
parseDigit = Parser $ nothingIf ((==) "" . snd) . swap . span isDigit

parseFail :: Parser a
parseFail = Parser $ const Nothing

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ nothingIf ((==) "" . snd) . swap . span f

parseSpanChar :: Char -> Parser String
parseSpanChar c = parseSpan (c ==)

parseString :: String -> Parser String
parseString s = Parser $ nothingIf ((/=) s . snd) . swap . splitAt (length s)

parseExcludeChars :: [Char] -> Parser String
parseExcludeChars l = parseSpan (and . (map (/=) l ??))