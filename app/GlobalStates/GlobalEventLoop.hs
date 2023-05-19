module GlobalStates.GlobalEventLoop where

import Data.Foldable (foldr1)
import Data.Map (alter, member, (!), (!?))
import Data.Maybe (fromMaybe)
import Graphics.Rendering.OpenGL (Color4, GLfloat)
import Parsers.CSSParser (Selector (ElementSelector), StyleMap (StyleMap))
import Parsers.ElementStyle (ElementStyle, backgroundStyle, (<|>>))
import Parsers.HTMLParser (Tag)
import Parsers.JSParser (ClickEvent (BackgroundColorClickEvent), ClickEvents)

updateBackground :: Tag -> Color4 GLfloat -> StyleMap -> StyleMap
updateBackground tag col (StyleMap styleMap) = newStyleMap
  where
    newBackground :: ElementStyle
    newBackground = backgroundStyle col
    newStyleMap :: StyleMap
    newStyleMap = StyleMap $ alter alterTagStyle (ElementSelector tag) styleMap
    alterTagStyle :: Maybe ElementStyle -> Maybe ElementStyle
    alterTagStyle Nothing = Just newBackground
    alterTagStyle (Just oldStyle) = Just (oldStyle <|>> newBackground)

combineBackgroundChange :: ClickEvents -> StyleMap -> StyleMap
combineBackgroundChange clickEvents = foldr1 (flip (.)) updateFunctionsList
  where
    updateBackground' :: ClickEvent -> StyleMap -> StyleMap
    updateBackground' (BackgroundColorClickEvent tag cols) = updateBackground tag cols
    updateFunctionsList :: [StyleMap -> StyleMap]
    updateFunctionsList = updateBackground' <$> clickEvents
