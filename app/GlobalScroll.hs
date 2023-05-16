module GlobalScroll where

import Data.StateVar (HasGetter (get), StateVar (StateVar), ($=))
import Foreign (malloc)
import Foreign.Ptr (Ptr)
import GHC.IO (unsafePerformIO)
import Graphics.UI.GLUT (Size (Size), windowSize)

{-
  TODO:
  - global scroll state ok ?
  - scroll speed ?
  - tablet ?
-}

data ScrollState = ScrollState
  { scrollPosition :: Int, -- pixels
    pageHeight :: Int -- pixels
  }

data CurrentScrollState = CurrentScrollState
  { scrollPosition' :: Ptr Int,
    pageHeight' :: Ptr Int
  }

currentScrollState :: StateVar ScrollState
{-# NOINLINE currentScrollState #-}
currentScrollState = StateVar getPageHeight setPageHeight
  where
    currentScrollState' :: CurrentScrollState
    currentScrollState' = unsafePerformIO $ do
      s <- malloc
      s $= 0
      h <- malloc
      h $= 0
      return $
        CurrentScrollState
          { scrollPosition' = s,
            pageHeight' = h
          }
    getPageHeight :: IO ScrollState
    getPageHeight = do
      s <- get $ scrollPosition' currentScrollState'
      h <- get $ pageHeight' currentScrollState'
      return
        ScrollState
          { scrollPosition = s,
            pageHeight = h
          }
    setPageHeight :: ScrollState -> IO ()
    setPageHeight (ScrollState s h) = do
      (Size _ wh) <- get windowSize
      scrollPosition' currentScrollState' $= max 0 (min s (h - fromIntegral wh))
      pageHeight' currentScrollState' $= h