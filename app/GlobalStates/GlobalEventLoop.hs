{-# LANGUAGE LambdaCase #-}

module GlobalStates.GlobalEventLoop where

import Data.Foldable (Foldable (foldMap'), foldr1)
import Data.IORef (IORef, newIORef)
import Data.List (partition)
import Data.Map (Map, alter, fromList, member, (!), (!?))
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import Graphics.Rendering.OpenGL (Color4, GLfloat)
import Parsers.CSSParser (Selector (ElementSelector), StyleMap (StyleMap))
import Parsers.ElementStyle (ElementStyle, backgroundStyle, colorStyle, (<|>>))
import Parsers.HTMLParser (Tag)
import Parsers.JSParser
  ( EventHandler (EventAnonymous, EventRefHandler),
    EventListChangeAction (AddEvent, RemoveEventRef),
    EventRef,
    EventRefAssignationAction (EventRefAssignationAction),
    Statement (..),
    Statements,
    StyleChangeAction (BackgroundColorChangeAction, ColorChangeAction),
  )

newtype EventRefMap = EventRefMap (Map EventRef CompiledEvent)

data Event = EventRef' EventRef | CompiledEvent' CompiledEvent

type EventList = [Event]

data CompiledEvent = CompiledEvent (EventList -> EventList) (StyleMap -> StyleMap)

--- Interpret the file

extractAssignations :: Statements -> (Statements, EventRefMap)
extractAssignations statements = (rest, eventRefMap)
  where
    (eventRefAssignations, rest) =
      partition
        ( \case
            EventRefAssignation _ -> True
            _ -> False
        )
        statements
    eventRefMap :: EventRefMap
    eventRefMap = EventRefMap $ fromList (eventRefStatementToTuple <$> eventRefAssignations)
    eventRefStatementToTuple :: Statement -> (EventRef, CompiledEvent)
    eventRefStatementToTuple
      (EventRefAssignation (EventRefAssignationAction eventRef statements)) =
        (eventRef, compileEvent statements)

compileStyleChange :: Statements -> StyleMap -> StyleMap
compileStyleChange statements = foldr1 (.) (compileStyleChange' <$> statements)
  where
    compileStyleChange' :: Statement -> StyleMap -> StyleMap
    compileStyleChange' (StyleChangeStatement styleChangeAction) (StyleMap styleMap) =
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
        alterTagStyle :: ElementStyle -> Maybe ElementStyle -> Maybe ElementStyle
        alterTagStyle newProperty Nothing = Just newProperty
        alterTagStyle newProperty (Just oldStyle) = Just (oldStyle <|>> newProperty)

compileEventListChange :: Statements -> EventList -> EventList
compileEventListChange statements = foldr1 (.) (compileEventListChange' <$> statements)
  where
    compileEventListChange' :: Statement -> EventList -> EventList
    compileEventListChange' (EventChangeStatement eventListChangeAction) = event
      where
        event :: EventList -> EventList
        event = case eventListChangeAction of
          AddEvent _ (EventRefHandler ref) -> applyWhen (any (refEquals ref)) (++ [EventRef' ref])
          AddEvent _ (EventAnonymous stts) ->
            (++ [CompiledEvent' $ compileEvent stts])
          RemoveEventRef _ ref -> filter (refEquals ref)
        refEquals :: EventRef -> Event -> Bool
        refEquals ref (EventRef' ref') = ref == ref'
        refEquals ref _ = False
        applyWhen :: (a -> Bool) -> (a -> a) -> a -> a
        applyWhen cd f x = if cd x then f x else x

compileEvent :: Statements -> CompiledEvent
compileEvent statements = CompiledEvent eventListChangeFunction styleChangeFunction
  where
    (styleChangeStatements, eventChangeStatements) =
      partition
        ( \case
            StyleChangeStatement _ -> True
            _ -> False
        )
        statements
    styleChangeFunction :: StyleMap -> StyleMap
    styleChangeFunction = compileStyleChange styleChangeStatements
    eventListChangeFunction :: EventList -> EventList
    eventListChangeFunction = compileEventListChange eventChangeStatements

currentStyleChange :: IORef (StyleMap -> StyleMap)
{-# NOINLINE currentStyleChange #-}
currentStyleChange = unsafePerformIO $ newIORef id