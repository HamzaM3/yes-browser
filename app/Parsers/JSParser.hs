module Parsers.JSParser where

import Graphics.Rendering.OpenGL (Color4, GLfloat)
import Parsers.HTMLParser (Tag, stringToTag)
import Parsers.ParserUtils.BaseParser (parseAlpha, parseChar, parseColorHex, parseEnd, parseString)
import Parsers.ParserUtils.Parser (Parser (..), parseMaxPossible, parseWhiteSpace, (*\\>), (</*), (<//*))

data ClickEvent = BackgroundColorClickEvent Tag (Color4 GLfloat)
  deriving (Show, Eq)

data Event = ClickEvent

type ClickEvents = [ClickEvent]

parseAddEventListenerMethod :: Parser ClickEvent
parseAddEventListenerMethod =
  parseString "document.addEventListener("
    *\\> parseString "\"click\""
    *\\> parseChar ','
    *\\> parseBackgroundColorLambda
    <//* parseChar ')'

parseClickString :: Parser String
parseClickString = parseString "\"click\""

parseComma :: Parser Char
parseComma = parseChar ','

parseBackgroundChangeStatement :: Parser ClickEvent
parseBackgroundChangeStatement =
  BackgroundColorClickEvent
    <$> (stringToTag <$> parseAlpha)
    <*> ( parseChar '.'
            *> parseString "style.backgroundColor"
              *\\> parseChar '='
              *\\> parseString "\"#"
            *> parseColorHex
            <* parseChar '\"'
              <//* parseChar ';'
        )

parseBackgroundColorLambda :: Parser ClickEvent
parseBackgroundColorLambda = parseLambdaStart *\\> parseBackgroundChangeStatement <//* parseLambdaEnd
  where
    parseLambdaStart :: Parser Char
    parseLambdaStart = parseChar '(' *\\> parseChar ')' *\\> parseString "=>" *\\> parseChar '{'
    -- Okay I see why tokenization
    parseLambdaEnd :: Parser Char
    parseLambdaEnd = parseChar '}'

parseBackgroundColorClickEvent :: Parser ClickEvent
parseBackgroundColorClickEvent = parseWhiteSpace *> parseAddEventListenerMethod </* parseChar ';'

parseBackgroundEventScript :: Parser ClickEvents
parseBackgroundEventScript = parseMaxPossible parseBackgroundColorClickEvent <//* parseEnd

mainJS :: IO ()
mainJS = do
  file <- readFile "script.js"
  let res = snd <$> runParser parseBackgroundEventScript file
  print res