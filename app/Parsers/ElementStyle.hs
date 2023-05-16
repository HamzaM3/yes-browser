module Parsers.ElementStyle where

import Control.Applicative (Alternative)
import GHC.Base (Alternative (..))
import Graphics.UI.GLUT (Color4 (Color4), GLfloat)

{-
  TODO:
  - default style sheet
  - more props
  - deal with inhertance or not
-}

data ElementStyle = ElementStyle
  { background :: Maybe (Color4 GLfloat),
    fontSize :: Maybe Int,
    color :: Maybe (Color4 GLfloat)
  }
  deriving (Show, Eq)

(<|>>) :: ElementStyle -> ElementStyle -> ElementStyle
(<|>>) e1 e2 =
  ElementStyle
    { background = background e2 <|> background e1,
      fontSize = fontSize e2 <|> fontSize e1,
      color = color e2 <|> color e1
    }

infixl 4 <|>>

emptyStyle :: ElementStyle
emptyStyle =
  ElementStyle
    { background = Nothing,
      fontSize = Nothing,
      color = Nothing
    }

backgroundStyle :: Color4 GLfloat -> ElementStyle
backgroundStyle col =
  emptyStyle
    { background = Just col
    }

fontSizeStyle :: Int -> ElementStyle
fontSizeStyle size =
  emptyStyle
    { fontSize = Just size
    }

colorStyle :: Color4 GLfloat -> ElementStyle
colorStyle col =
  emptyStyle
    { color = Just col
    }

defaultStyle :: ElementStyle
defaultStyle =
  ElementStyle
    { background = Just $ Color4 1 1 1 1,
      fontSize = Just 16,
      color = Just $ Color4 0 0 0 1
    }