{-# OPTIONS_GHC -Wno-missing-fields #-}

module CSSParser where

import Control.Applicative (Alternative ((<|>)), empty)
import Data.Char (ord)
import Data.List.Unique
import Data.Map (Map, singleton, unionsWith)
import ElementStyle
import Graphics.GL.Types (GLfloat)
import Graphics.UI.GLUT (Color4 (Color4))
import HTMLParser
import ParserUtils.BaseParser
import ParserUtils.CheckParser
import ParserUtils.Parser
  ( Parser (runParser),
    parseMaxPossible,
    parseWhiteSpace,
    (*\>),
    (*\\>),
    (</*),
    (<//*),
    (<//*>),
  )
import ParserUtils.ParsingUtils (hexToColor4, isAuthorized)

newtype Selector = ElementSelector Tag
  deriving (Show, Eq, Ord)

data Declaration = Background (Color4 GLfloat) | FontSize Int | Color (Color4 GLfloat)
  deriving (Show, Eq)

type Declarations = [Declaration]

type Selectors = [Selector]

newtype DeclarationBlock = DeclarationBlock Declarations
  deriving (Show, Eq)

data Rule = Rule Selectors DeclarationBlock
  deriving (Show, Eq)

type Rules = [Rule]

newtype StyleSheet = StyleSheet Rules
  deriving (Show, Eq)

newtype StyleMap = StyleMap (Map Selector ElementStyle)
  deriving (Show, Eq)

parseColorHex :: Parser (Color4 GLfloat)
parseColorHex = hexToColor4 <$> (parseSpan (isAuthorized "1234567890ABCDEFabcdef") >>= checkLength 6)

parseSelector :: Parser Selector
parseSelector = ElementSelector . stringToTag <$> parseAlpha

parseSelectors :: Parser Selectors
parseSelectors = (:) <$> parseSelector <*> parseMaxPossible (parseWhiteSpace *> parseChar ',' *\\> parseSelector)

parseBackground :: Parser Declaration
parseBackground =
  Background
    <$> ( parseWhiteSpace
            *> parseString "background"
              *\\> parseChar ':'
              *\\> parseChar '#'
            *> parseColorHex
              <//* parseChar ';'
        )

parseFontSize :: Parser Declaration
parseFontSize =
  FontSize . read
    <$> ( parseWhiteSpace
            *> parseString "font-size"
              *\> parseChar ':'
              *\> parseDigit
            <* parseString "px"
              </* parseChar ';'
        )

parseColor :: Parser Declaration
parseColor =
  Color
    <$> ( parseWhiteSpace
            *> parseString "color"
              *\\> parseChar ':'
              *\\> parseChar '#'
            *> parseColorHex
              <//* parseChar ';'
        )

parseDeclaration :: Parser Declaration
parseDeclaration = parseBackground <|> parseColor <|> parseFontSize

parseDeclarationBlock :: Parser DeclarationBlock
parseDeclarationBlock =
  DeclarationBlock
    <$> ( parseWhiteSpace *\\> parseChar '{' *\\> parseMaxPossible parseDeclaration <//* parseChar '}'
        )

parseRule :: Parser Rule
parseRule = parseWhiteSpace *\\> (Rule <$> parseSelectors <//*> parseDeclarationBlock)

parseStyleSheet :: Parser StyleSheet
parseStyleSheet = StyleSheet <$> parseMaxPossible parseRule <//* parseEnd

testCSS :: String
-- testCSS = "background: #AA3AAC;"
-- testCSS = "font-size: 14px;"
-- testCSS = "p, div, title{color: #AAC342;}"
testCSS = "p{font-size: 13px; font-size: 14px;} p {font-size:12px;}"

mainCSS :: IO ()
mainCSS = do
  -- css <- readFile "style.css"
  -- let Just res = snd <$> runParser parseStyleSheet testCSS
  -- print $ cascadeSheets res
  let Just res = snd <$> runParser parseStyleSheet testCSS
  print res
  print $ cascadeSheets res

getSelectors :: Rules -> [Selector]
getSelectors rules = uniq $ concatMap extractSelectors rules
  where
    extractSelectors (Rule sel _) = sel

summarizeDeclarations :: Declarations -> ElementStyle
summarizeDeclarations [] = emptyStyle
summarizeDeclarations (Background col : rest) = backgroundStyle col <|>> summarizeDeclarations rest
summarizeDeclarations (FontSize size : rest) = fontSizeStyle size <|>> summarizeDeclarations rest

summarizeDeclarationBlock :: DeclarationBlock -> ElementStyle
summarizeDeclarationBlock (DeclarationBlock decls) = summarizeDeclarations decls

cascadeSheets :: StyleSheet -> StyleMap
cascadeSheets (StyleSheet rules) = StyleMap $ unionsWith (<|>>) $ concatMap addSelectors rules
  where
    addSelectors :: Rule -> [Map Selector ElementStyle]
    addSelectors (Rule sels declBlock) = Prelude.map (`singleton` elementStyle) sels
      where
        elementStyle = summarizeDeclarationBlock declBlock
