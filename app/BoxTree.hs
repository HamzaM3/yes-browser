module BoxTree where

import Data.Map ((!))
import Data.Map.Lazy (member)
import Data.Maybe (fromJust)
import Foreign (Ptr, allocaBytes, peekArray)
import Foreign.C (CFloat, CString, withCString)
import GHC.IO.Unsafe (unsafePerformIO)
import Graphics.Rendering.FTGL
  ( Layout,
    createSimpleLayout,
    createTextureFont,
    getFontLineHeight,
    setFontFaceSize,
    setLayoutFont,
    setLayoutLineLength,
  )
import Parsers.CSSParser (Selector (ElementSelector), StyleMap (..))
import Parsers.ElementStyle (ElementStyle (fontSize), emptyStyle, (<|>>))
import Parsers.HTMLParser (HTMLDOM (..), parsePage)
import Parsers.ParserUtils.Parser (Parser)

{-
  TODO:
    - remove default value and allow css to fill them
    - improve the data structure
      - tag might be needed for JS manipulation
      - deal with input and buttons (maybe ?)
    - make sure there is always a value for all properties
      - default value
      - deal with inheritable values and non-inheritable
    - improve text box height calculation
      - we have ftgl so modify the lib in order to do that
    - how to style text box
      - I think it should inherit from its parent style (we don't css text boxes)
    - Leaf => ShallowEmptyNode ?
    - margin padding width height min-w min-h elementwise
      - how are they actually specified ?
    - flex grid
    - relative units
    - position, floats
    - selector => element style
    - giga func of type :: css file -> html file -> display tree
-}

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

data ShallowTree = Leaf ElementStyle | ShallowTextNode String | ShallowNode ElementStyle [ShallowTree]
  deriving (Eq, Show)

-- (width, height)
type Dimension = (Int, Int)

-- (x, y)
type CornerPosition = (Int, Int)

data Box = Box Dimension CornerPosition
  deriving (Eq, Show)

data DisplayTree = DisplayLeaf Box ElementStyle String | DisplayNode Box ElementStyle [DisplayTree]
  deriving (Eq, Show)

type FontPath = String

-- temporary global style

minH :: Int
minH = 30

padding :: Int
padding = 10

margin :: Int
margin = 10

-- fontPath elementStyle width s
getTextHeight :: FontPath -> ElementStyle -> Int -> String -> Float
getTextHeight fontPath elementStyle width s = unsafePerformIO $ do
  font <- createTextureFont fontPath
  let fontSize = fromJust $ Parsers.ElementStyle.fontSize elementStyle
  setFontFaceSize font fontSize fontSize

  layout <- createSimpleLayout
  setLayoutFont layout font
  setLayoutLineLength layout (fromIntegral width)
  let v = getFontLineHeight font
  let u = getLayoutHeight layout s
  return $ getAboveMultiple v u

getAboveMultiple :: Float -> Float -> Float
getAboveMultiple x y = x * fromIntegral (ceiling (y / x))

shallowToDisplay :: ElementStyle -> FontPath -> CornerPosition -> Int -> ShallowTree -> DisplayTree
shallowToDisplay elementStyle fontPath (x, y) width = shallowToDisplay'
  where
    shallowToDisplay' (Leaf elementStyle') =
      DisplayLeaf
        (Box (width, minH) (x, y))
        (elementStyle <|>> elementStyle')
        ""
    shallowToDisplay' (ShallowTextNode s) =
      DisplayLeaf
        (Box (width, ceiling textNodeHeight) (x, y))
        (emptyStyle <|>> elementStyle)
        s
      where
        textNodeHeight :: Float
        textNodeHeight = getTextHeight fontPath elementStyle width s
    shallowToDisplay' (ShallowNode elementStyle' []) =
      DisplayNode
        (Box (width, minH) (x, y))
        (elementStyle <|>> elementStyle')
        []
    shallowToDisplay' (ShallowNode elementStyle' children) =
      DisplayNode
        (Box rootDimension (x, y))
        (elementStyle <|>> elementStyle')
        displayTrees
      where
        getNextSiblingCorner :: DisplayTree -> CornerPosition
        getNextSiblingCorner (DisplayLeaf (Box (_, h) (x, 9)) _ _) = (x, y + h + margin)
        getNextSiblingCorner (DisplayNode (Box (_, h) (x, y)) _ _) = (x, y + h + margin)
        displayTrees :: [DisplayTree]
        displayTrees = Prelude.foldl addNext [headDisplay] (tail children)
          where
            headDisplay :: DisplayTree
            headDisplay =
              shallowToDisplay
                (elementStyle <|>> elementStyle')
                fontPath
                (x + padding, y + padding)
                (width - 2 * padding)
                (head children)
            addNext :: [DisplayTree] -> ShallowTree -> [DisplayTree]
            addNext listDisplayTree shallowTree =
              listDisplayTree
                ++ [ shallowToDisplay
                       (elementStyle <|>> elementStyle')
                       fontPath
                       (x', y')
                       (width - 2 * padding)
                       shallowTree
                   ]
              where
                (x', y') = getNextSiblingCorner (last listDisplayTree)
        rootDimension :: CornerPosition
        rootDimension = (width, height)
          where
            height = getLowerY (last displayTrees) + padding - y
        getLowerY :: DisplayTree -> Int
        getLowerY (DisplayLeaf (Box (_, h) (_, y)) _ _) = y + h
        getLowerY (DisplayNode (Box (_, h) (_, y)) _ _) = y + h

htmlToShallow :: StyleMap -> HTMLDOM -> ShallowTree
htmlToShallow (StyleMap styleMap) = htmlToShallow'
  where
    htmlToShallow' (SelfClosed tag) = ShallowTextNode ""
    htmlToShallow' (TextNode s) = ShallowTextNode s
    htmlToShallow' (Node tag children) = ShallowNode elementStyle (htmlToShallow' <$> children)
      where
        elementStyle = if member (ElementSelector tag) styleMap then styleMap ! ElementSelector tag else emptyStyle

parseDisplay :: StyleMap -> FontPath -> CornerPosition -> Int -> Parser DisplayTree
parseDisplay styleMap font corner width = shallowToDisplay emptyStyle font corner width . htmlToShallow styleMap <$> parsePage
