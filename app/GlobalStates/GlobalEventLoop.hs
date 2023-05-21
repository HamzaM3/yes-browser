module GlobalStates.GlobalEventLoop where

import Data.Foldable (foldr1)
import Data.IORef (IORef, newIORef)
import Data.Map (alter, member, (!), (!?))
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import Graphics.Rendering.OpenGL (Color4, GLfloat)
import Parsers.CSSParser (Selector (ElementSelector), StyleMap (StyleMap))
import Parsers.ElementStyle (ElementStyle, backgroundStyle, colorStyle, (<|>>))
import Parsers.HTMLParser (Tag)
import Parsers.JSParser
  ( Event (..),
    Events,
    StyleChangeAction (..),
  )

alterTagStyle :: ElementStyle -> Maybe ElementStyle -> Maybe ElementStyle
alterTagStyle newProperty Nothing = Just newProperty
alterTagStyle newProperty (Just oldStyle) = Just (oldStyle <|>> newProperty)

updateStyle :: StyleChangeAction -> StyleMap -> StyleMap
updateStyle styleChangeAction (StyleMap styleMap) =
  StyleMap $
    alter
      (alterTagStyle newProperty)
      (ElementSelector tag)
      styleMap
  where
    newProperty :: ElementStyle
    newProperty = case styleChangeAction of
      (BackgroundColorChangeAction _ col) -> backgroundStyle col
      (ColorChangeAction _ col) -> colorStyle col
    tag :: Tag
    tag = case styleChangeAction of
      (BackgroundColorChangeAction tag _) -> tag
      (ColorChangeAction tag _) -> tag

combineStyleChange :: Events -> StyleMap -> StyleMap
combineStyleChange clickEvents = foldr1 (flip (.)) updateFunctionsList
  where
    patternMatch :: Event -> [StyleChangeAction]
    patternMatch (ClickEvent actionList) = actionList
    updateFunctionsList :: [StyleMap -> StyleMap]
    updateFunctionsList = concatMap (updateStyle <$>) (patternMatch <$> clickEvents)

currentStyleChange :: IORef (StyleMap -> StyleMap)
{-# NOINLINE currentStyleChange #-}
currentStyleChange = unsafePerformIO $ newIORef id