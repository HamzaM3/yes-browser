module BoxTree where

import Control.Monad (forM_)
import Data.Foldable (foldl')
import Data.List.NonEmpty (fromList, toList)
import Data.Monoid (Sum (..))
import Data.Random
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import FreeType
import GHC.Base
import GHC.IO.Unsafe (unsafePerformIO)
import Graphics.Rendering.FTGL
  ( Font,
    Layout,
    RenderMode (All),
    createSimpleLayout,
    createTextureFont,
    destroyFont,
    destroyLayout,
    getFontAscender,
    getFontDescender,
    getFontError,
    getFontLineHeight,
    renderFont,
    renderLayout,
    setFontFaceSize,
    setLayoutFont,
    setLayoutLineLength,
  )
import Graphics.UI.GLUT
import HTMLParser
import ParserUtils.Parser
import System.Random.MWC (Gen, create)

foreign import ccall unsafe "ftglGetLayoutBBox"
  ftglGetLayoutBBox :: Graphics.Rendering.FTGL.Layout -> CString -> Ptr CFloat -> IO ()

getLayoutBBox :: Graphics.Rendering.FTGL.Layout -> String -> IO [Float]
getLayoutBBox l s = allocaBytes 24 $ \pf ->
  withCString s $ \ps -> do
    ftglGetLayoutBBox l ps pf
    map realToFrac <$> peekArray 6 pf

getLayoutHeight :: Graphics.Rendering.FTGL.Layout -> String -> Float
getLayoutHeight l s = unsafePerformIO $ do
  [_, y1, _, _, y2, _] <- getLayoutBBox l s
  return $ y2 - y1

getLayoutWidth :: Graphics.Rendering.FTGL.Layout -> String -> Float
getLayoutWidth l s = unsafePerformIO $ do
  [x1, _, _, x2, _, _] <- getLayoutBBox l s
  return $ x2 - x1

data ShallowTree = Leaf | ShallowTextNode String | ShallowNode [ShallowTree]
  deriving (Eq, Show)

-- (width, height)
type Dimension = (Int, Int)

-- (x, y)
type CornerPosition = (Int, Int)

data Box = Box Dimension CornerPosition
  deriving (Eq, Show)

data DisplayTree = DisplayLeaf Box String | DisplayNode Box [DisplayTree]
  deriving (Eq, Show)

minH :: Int
minH = 30

padding :: Int
padding = 10

fontSize :: Int
fontSize = 24

margin :: Int
margin = 10

getAboveMultiple :: Float -> Float -> Float
getAboveMultiple x y = x * fromIntegral (ceiling (y / x))

getXYs :: Graphics.Rendering.FTGL.Font -> CornerPosition -> Int -> ShallowTree -> DisplayTree
getXYs font (x, y) width Leaf = DisplayLeaf (Box (width, minH) (x, y)) ""
getXYs font (x, y) width (ShallowTextNode s) = DisplayLeaf (Box (width, ceiling h) (x, y)) s
  where
    h :: Float
    h = unsafePerformIO $ do
      layout <- createSimpleLayout
      setLayoutFont layout font
      setLayoutLineLength layout (fromIntegral width)
      let v = getFontLineHeight font
      let u = getLayoutHeight layout s
      return $ getAboveMultiple v u
getXYs font (x, y) width (ShallowNode []) = DisplayNode (Box (width, minH) (x, y)) []
getXYs font (x, y) width (ShallowNode children) = DisplayNode (Box rootDimension (x, y)) displays
  where
    getNextSiblingCorner :: DisplayTree -> CornerPosition
    getNextSiblingCorner (DisplayLeaf (Box (_, h) (x, y)) _) = (x, y + h + margin)
    getNextSiblingCorner (DisplayNode (Box (_, h) (x, y)) _) = (x, y + h + margin)
    displays :: [DisplayTree]
    displays = Prelude.foldr addNext [headDisplay] (tail children)
      where
        headDisplay :: DisplayTree
        headDisplay = getXYs font (x + padding, y + padding) (width - 2 * padding) (head children)
        addNext :: ShallowTree -> [DisplayTree] -> [DisplayTree]
        addNext d l = let (x', y') = getNextSiblingCorner (last l) in l ++ [getXYs font (x', y') (width - 2 * padding) d]
    rootDimension :: CornerPosition
    rootDimension = (width, height)
      where
        height = getLowerY (last displays) + padding - y
    getLowerY :: DisplayTree -> Int
    getLowerY (DisplayLeaf (Box (_, h) (_, y)) _) = y + h
    getLowerY (DisplayNode (Box (_, h) (_, y)) _) = y + h

htmlToShallow :: HTMLDOM -> ShallowTree
htmlToShallow (SelfClosed _) = ShallowTextNode ""
htmlToShallow (TextNode s) = ShallowTextNode s
htmlToShallow (Node _ children) = ShallowNode (htmlToShallow <$> children)

parseDisplay :: Graphics.Rendering.FTGL.Font -> CornerPosition -> Int -> Parser DisplayTree
parseDisplay font corner width = getXYs font corner width . htmlToShallow <$> parsePage
