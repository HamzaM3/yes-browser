module GlobalStyle where

import Data.Maybe (fromJust)
import Data.StateVar (HasGetter (get), StateVar (..), ($=))
import ElementStyle (ElementStyle (ElementStyle), defaultStyle)
import qualified ElementStyle (background, color, fontSize)
import Foreign (Ptr, Storable, malloc)
import GHC.IO (unsafePerformIO)
import Graphics.Rendering.OpenGL (Color4 (..), GLfloat)

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

currentStyle :: StateVar ElementStyle.ElementStyle
{-# NOINLINE currentStyle #-}
currentStyle = StateVar getStyle assignStyle
  where
    currentStyle' :: CurrentStyle
    {-# NOINLINE currentStyle' #-}
    currentStyle' = unsafePerformIO $ do
      b <- malloc' $ fromJust $ ElementStyle.background defaultStyle
      f <- malloc' $ fromJust $ ElementStyle.fontSize defaultStyle
      c <- malloc' $ fromJust $ ElementStyle.color defaultStyle
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
    assignStyle :: ElementStyle.ElementStyle -> IO ()
    assignStyle elementStyle = do
      mapM_ (background currentStyle' $=) $ ElementStyle.background elementStyle
      mapM_ (fontSize currentStyle' $=) $ ElementStyle.fontSize elementStyle
      mapM_ (color currentStyle' $=) $ ElementStyle.color elementStyle
    getStyle :: IO ElementStyle.ElementStyle
    getStyle = do
      b <- get $ background currentStyle'
      f <- get $ fontSize currentStyle'
      c <- get $ color currentStyle'
      return $
        ElementStyle
          { ElementStyle.background = Just b,
            ElementStyle.fontSize = Just f,
            ElementStyle.color = Just c
          }

withStyle :: ElementStyle -> IO () -> IO ()
withStyle elementStyle action = do
  oldStyle <- get currentStyle
  currentStyle $= elementStyle
  action
  currentStyle $= oldStyle