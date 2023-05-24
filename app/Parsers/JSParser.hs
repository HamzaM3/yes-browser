{-# LANGUAGE GADTs #-}

module Parsers.JSParser where

import Control.Applicative ((<|>))
import Data.List (partition)
import Data.Map (Map, fromList, (!))
import Graphics.Rendering.OpenGL (Color4, GLfloat, ListMode (Compile))
import Parsers.CSSParser (StyleMap)
import Parsers.HTMLParser (Tag, stringToTag)
import Parsers.ParserUtils.BaseParser (parseAlpha, parseChar, parseColorHex, parseEnd, parseString)
import Parsers.ParserUtils.Parser (Parser (..), parseMaxPossibleWithWhiteSpace, parseWhiteSpace, (*\\>), (</*), (<//*), (<//*>))

--- Parse file

type PropertyName = String

type EventRef = String

data EventType = ClickEvent
  deriving (Show, Eq)

data EventListChangeAction = AddEvent EventType EventHandler | RemoveEventRef EventType EventRef
  deriving (Show, Eq)

data StyleChangeAction
  = BackgroundColorChangeAction Tag (Color4 GLfloat)
  | ColorChangeAction Tag (Color4 GLfloat)
  deriving (Show, Eq)

data EventRefAssignationAction
  = EventRefAssignationAction EventRef Statements
  deriving (Show, Eq)

data Statement
  = StyleChangeStatement StyleChangeAction
  | EventChangeStatement EventListChangeAction
  | EventRefAssignation EventRefAssignationAction
  deriving (Show, Eq)

type Statements = [Statement]

data EventHandler = EventRefHandler EventRef | EventAnonymous Statements
  deriving (Show, Eq)

--- StyleChangeAction

parseTagStyleChangeStatement ::
  (Tag -> Color4 GLfloat -> StyleChangeAction) ->
  PropertyName ->
  Parser StyleChangeAction
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

parseBackgroundChangeStatement :: Parser StyleChangeAction
parseBackgroundChangeStatement = parseTagStyleChangeStatement BackgroundColorChangeAction "backgroundColor"

parseColorChangeStatement :: Parser StyleChangeAction
parseColorChangeStatement = parseTagStyleChangeStatement ColorChangeAction "color"

parseStyleChangeAction :: Parser StyleChangeAction
parseStyleChangeAction = parseBackgroundChangeStatement <|> parseColorChangeStatement

--- EventListChangeAction

parseAddEventListenerMethod :: Parser EventListChangeAction
parseAddEventListenerMethod =
  AddEvent
    <$> ( parseString "document.addEventListener("
            *\\> parseChar '"'
            *> parseEventType
            <* parseChar '"'
              <//* parseChar ','
        )
      <//*> parseEventHandler
      <//* parseChar ')'

parseRemoveEventListenerMethod :: Parser EventListChangeAction
parseRemoveEventListenerMethod =
  RemoveEventRef
    <$> ( parseString "document.removeEventListener("
            *\\> parseChar '"'
            *> parseEventType
            <* parseChar '"'
              <//* parseChar ','
        )
      <//*> parseEventRef
      <//* parseChar ')'

parseEventListChangeStatement :: Parser EventListChangeAction
parseEventListChangeStatement = parseAddEventListenerMethod <|> parseRemoveEventListenerMethod

--- EventRefAssignationAction

parseEventRefAssignation :: Parser EventRefAssignationAction
parseEventRefAssignation =
  EventRefAssignationAction
    <$> ( parseString "const"
            *\\> parseAlpha
            <//* parseChar '='
        )
      <//*> parseAnonymous

--- Statement

parseStatement :: Parser Statement
parseStatement =
  (StyleChangeStatement <$> parseStyleChangeAction)
    <|> (EventChangeStatement <$> parseEventListChangeStatement)

parseStatements :: Parser Statements
parseStatements = parseMaxPossibleWithWhiteSpace parseStatement

--- Anonymous

parseAnonymous :: Parser Statements
parseAnonymous =
  parseLambdaStart
    *\\> parseStatements
    <//* parseLambdaEnd
  where
    parseLambdaStart :: Parser Char
    parseLambdaStart = parseChar '(' *\\> parseChar ')' *\\> parseString "=>" *\\> parseChar '{'
    -- Okay I see why tokenization
    parseLambdaEnd :: Parser Char
    parseLambdaEnd = parseChar '}'

--- EventRef

parseEventRef :: Parser EventRef
parseEventRef = parseAlpha

--- EventType

parseEventType :: Parser EventType
parseEventType = ClickEvent <$ parseString "click"

parseEventHandler :: Parser EventHandler
parseEventHandler = (EventRefHandler <$> parseEventRef) <|> (EventAnonymous <$> parseAnonymous)

parseScript :: Parser Statements
parseScript =
  parseMaxPossibleWithWhiteSpace
    ( parseStatement
        <|> (EventRefAssignation <$> parseEventRefAssignation)
    )
    <//* parseEnd

mainJS :: IO ()
mainJS = do
  file <- readFile "script.js"
  let res =
        snd
          <$> runParser parseScript file
  print res