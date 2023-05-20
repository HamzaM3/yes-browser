module GlobalStates.GlobalEventLoop where

import Data.Foldable (foldr1)
import Data.Map (alter, member, (!), (!?))
import Data.Maybe (fromMaybe)
import Graphics.Rendering.OpenGL (Color4, GLfloat)
import Parsers.CSSParser (Selector (ElementSelector), StyleMap (StyleMap))
import Parsers.ElementStyle (ElementStyle, backgroundStyle, colorStyle, (<|>>))
import Parsers.HTMLParser (Tag)
import Parsers.JSParser

alterTagStyle :: ElementStyle -> Maybe ElementStyle -> Maybe ElementStyle
alterTagStyle newProperty Nothing = Just newProperty
alterTagStyle newProperty (Just oldStyle) = Just (oldStyle <|>> newProperty)

updateStyle :: StyleChangeAction -> StyleMap -> StyleMap
updateStyle (BackgroundColorChangeAction tag col) (StyleMap styleMap) =
  StyleMap $
    alter
      (alterTagStyle (backgroundStyle col))
      (ElementSelector tag)
      styleMap
updateStyle (ColorChangeAction tag col) (StyleMap styleMap) =
  StyleMap $
    alter
      (alterTagStyle (colorStyle col))
      (ElementSelector tag)
      styleMap
  where
    newProperty :: StyleChangeAction -> ElementStyle
    newProperty (BackgroundColorChangeAction _ col) = backgroundStyle col
    newProperty (ColorChangeAction _ col) = colorStyle col

combineBackgroundChange :: Events -> StyleMap -> StyleMap
combineBackgroundChange clickEvents = foldr1 (flip (.)) updateFunctionsList
  where
    patternMatch :: Event -> [StyleChangeAction]
    patternMatch (ClickEvent actionList) = actionList
    updateFunctionsList :: [StyleMap -> StyleMap]
    updateFunctionsList = concatMap (updateStyle <$>) (patternMatch <$> clickEvents)
