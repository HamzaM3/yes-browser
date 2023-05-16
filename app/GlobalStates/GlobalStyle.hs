module GlobalStates.GlobalStyle where

import Data.Maybe (fromJust)
import Data.StateVar (HasGetter (get), StateVar (..), ($=))
import Foreign (Ptr, Storable, malloc)
import GHC.IO (unsafePerformIO)
import Graphics.Rendering.OpenGL (Color4 (..), GLfloat)
import Parsers.ElementStyle
  ( ElementStyle (ElementStyle),
    defaultStyle,
  )
import qualified Parsers.ElementStyle
  ( background,
    color,
    fontSize,
  )

{-
  TODO:
  - is global var ok ?
    - how about tabs ?
  - default style dealt with
-}

data CurrentStyle = CurrentStyle
  { background :: Ptr (Color4 GLfloat),
    fontSize :: Ptr Int,
    color :: Ptr (Color4 GLfloat)
  }
  deriving (Eq)

currentStyle :: StateVar Parsers.ElementStyle.ElementStyle
{-# NOINLINE currentStyle #-}
currentStyle = StateVar getStyle assignStyle
  where
    currentStyle' :: CurrentStyle
    {-# NOINLINE currentStyle' #-}
    currentStyle' = unsafePerformIO $ do
      b <- malloc' $ fromJust $ Parsers.ElementStyle.background defaultStyle
      f <- malloc' $ fromJust $ Parsers.ElementStyle.fontSize defaultStyle
      c <- malloc' $ fromJust $ Parsers.ElementStyle.color defaultStyle
      return $
        CurrentStyle
          { background = b,
            fontSize = f,
            color = c
          }
      where
        malloc' :: (Storable a) => a -> IO (Ptr a)
        malloc' x = do
          c <- malloc
          c $= x
          return c
    assignStyle :: Parsers.ElementStyle.ElementStyle -> IO ()
    assignStyle elementStyle = do
      mapM_ (background currentStyle' $=) $ Parsers.ElementStyle.background elementStyle
      mapM_ (fontSize currentStyle' $=) $ Parsers.ElementStyle.fontSize elementStyle
      mapM_ (color currentStyle' $=) $ Parsers.ElementStyle.color elementStyle
    getStyle :: IO Parsers.ElementStyle.ElementStyle
    getStyle = do
      b <- get $ background currentStyle'
      f <- get $ fontSize currentStyle'
      c <- get $ color currentStyle'
      return $
        ElementStyle
          { Parsers.ElementStyle.background = Just b,
            Parsers.ElementStyle.fontSize = Just f,
            Parsers.ElementStyle.color = Just c
          }

withStyle :: ElementStyle -> IO () -> IO ()
withStyle elementStyle action = do
  oldStyle <- get currentStyle
  currentStyle $= elementStyle
  action
  currentStyle $= oldStyle