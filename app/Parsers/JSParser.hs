module Parsers.JSParser where

import Control.Applicative ((<|>))
import Graphics.Rendering.OpenGL (Color4, GLfloat)
import Parsers.HTMLParser (Tag, stringToTag)
import Parsers.ParserUtils.BaseParser (parseAlpha, parseChar, parseColorHex, parseEnd, parseString)
import Parsers.ParserUtils.Parser (Parser (..), parseMaxPossibleWithWhiteSpace, parseWhiteSpace, (*\\>), (</*), (<//*))

data StyleChangeAction = BackgroundColorChangeAction Tag (Color4 GLfloat) | ColorChangeAction Tag (Color4 GLfloat)
  deriving (Show, Eq)

newtype Event = ClickEvent [StyleChangeAction]
  deriving (Show, Eq)

type Events = [Event]

type PropertyName = String

parseBackgroundChangeStatement :: Parser StyleChangeAction
parseBackgroundChangeStatement = parseTagStyleChangeStatement BackgroundColorChangeAction "backgroundColor"

parseColorChangeStatement :: Parser StyleChangeAction
parseColorChangeStatement = parseTagStyleChangeStatement ColorChangeAction "color"

parseStyleChangeStatement :: Parser StyleChangeAction
parseStyleChangeStatement = parseBackgroundChangeStatement <|> parseColorChangeStatement

parseTagStyleChangeStatement :: (Tag -> Color4 GLfloat -> StyleChangeAction) -> PropertyName -> Parser StyleChangeAction
parseTagStyleChangeStatement constructor property =
  constructor
    <$> (stringToTag <$> parseAlpha)
    <*> ( parseString (".style." ++ property)
            *\\> parseChar '='
            *\\> parseString "\"#"
            *> parseColorHex
            <* parseChar '\"'
              <//* parseChar ';'
        )

parseStyleChangeLambda :: Parser [StyleChangeAction]
parseStyleChangeLambda =
  parseLambdaStart
    *\\> parseMaxPossibleWithWhiteSpace parseStyleChangeStatement
    <//* parseLambdaEnd
  where
    parseLambdaStart :: Parser Char
    parseLambdaStart = parseChar '(' *\\> parseChar ')' *\\> parseString "=>" *\\> parseChar '{'
    -- Okay I see why tokenization
    parseLambdaEnd :: Parser Char
    parseLambdaEnd = parseChar '}'

parseAddEventListenerMethod :: Parser Event
parseAddEventListenerMethod =
  ClickEvent
    <$> ( parseString "document.addEventListener("
            *\\> parseString "\"click\""
            *\\> parseChar ','
            *\\> parseStyleChangeLambda
            <//* parseChar ')'
        )

parseStyleChangeClickEvent :: Parser Event
parseStyleChangeClickEvent = parseAddEventListenerMethod </* parseChar ';'

parseScript :: Parser Events
parseScript = parseMaxPossibleWithWhiteSpace parseStyleChangeClickEvent <//* parseEnd

mainJS :: IO ()
mainJS = do
  file <- readFile "script.js"
  let res =
        snd
          <$> runParser parseScript file
  print res