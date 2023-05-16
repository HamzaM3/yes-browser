module Display where

import BoxTree
  ( Box (..),
    CornerPosition,
    Dimension,
    DisplayTree (..),
    FontPath,
    parseDisplay,
  )
import Control.Monad (forM_)
import Data.Maybe (fromJust, fromMaybe)
import Data.StateVar
  ( HasGetter (get),
    HasSetter (($=)),
  )
import GHC.IO.Unsafe (unsafePerformIO)
import GlobalStates.GlobalScroll (ScrollState (ScrollState, scrollPosition), currentScrollState)
import GlobalStates.GlobalStyle (currentStyle, withStyle)
import Graphics.Rendering.FTGL
  ( Font,
    Layout,
    RenderMode (All),
    createSimpleLayout,
    createTextureFont,
    destroyFont,
    destroyLayout,
    getFontAscender,
    getFontLineHeight,
    renderFont,
    renderLayout,
    setFontFaceSize,
    setLayoutFont,
    setLayoutLineLength,
  )
import Graphics.UI.GLUT
  ( ClearBuffer (ColorBuffer),
    Color (color),
    Color4 (Color4),
    DisplayCallback,
    DisplayMode (RGBMode, SingleBuffered),
    GLfloat,
    MatrixComponent (translate),
    MatrixMode (Modelview, Projection),
    MouseWheelCallback,
    Position (Position),
    Rect (rect),
    ReshapeCallback,
    Size (Size),
    Vector3 (..),
    Vertex2 (Vertex2),
    clear,
    clearColor,
    createWindow,
    currentColor,
    displayCallback,
    flush,
    initialDisplayMode,
    initialWindowSize,
    initialize,
    loadIdentity,
    mainLoop,
    matrixMode,
    mouseWheelCallback,
    ortho2D,
    postRedisplay,
    reshapeCallback,
    viewport,
    windowSize,
  )
import Parsers.CSSParser
  ( StyleMap,
    StyleSheet,
    cascadeSheets,
    parseStyleSheet,
  )
import Parsers.ElementStyle (ElementStyle (background, color, fontSize))
import Parsers.HTMLParser (parsePage)
import Parsers.ParserUtils.Parser

{-
  TODO:
  - is negative y necessary ?
    - how to get to normal stuff (I believe it's the easiest)
  - better rerender (on scroll particularly)
    - no postredisplay
    - improve perf
  - interactivity
  - more fonts
  - improve ftgl
  - don't recreate fonts in order to get height
    - where does this bug come from
  - big problem ! destroy font and layout
    - maybe withFont/withLayout
  - window size dependence
  - fix namespacing (the lesser the better)
-}

type HTMLFile = String

type CSSFile = String

windowInitWidth :: Int
windowInitWidth = 1200

windowInitHeight :: Int
windowInitHeight = 800

initializeGL :: IO ()
initializeGL = do
  let progName = "Yes Browser"
  initialize progName []
  initialDisplayMode $= [SingleBuffered, RGBMode]
  initialWindowSize $= Size (fromIntegral windowInitWidth) (fromIntegral windowInitHeight)
  createWindow progName
  clearColor $= Color4 1 1 1 1.0

getFont :: FontPath -> Int -> IO Graphics.Rendering.FTGL.Font
getFont fontPath size = do
  font <- createTextureFont fontPath
  setFontFaceSize font size size
  return font

showText :: FontPath -> CornerPosition -> Dimension -> String -> IO ()
showText fontPath (x, y) (w, h) text = do
  style <- get currentStyle
  let fontSize = fromJust $ Parsers.ElementStyle.fontSize style -- fontsize has to be defined (=> remove the Maybe)
  (font, layout) <- getFontAndLayout fontSize
  let asc = getFontAscender font

  mapM_ Graphics.UI.GLUT.color $ Parsers.ElementStyle.color style -- maybe change color
  moveCursor asc x y
  renderLayout layout text
  moveCursorBack asc x y

  destroyFont font -- bad bad bad
  destroyLayout layout
  where
    moveCursor :: Float -> Int -> Int -> IO ()
    moveCursor asc x y = do
      translate (Vector3 0 (-asc) 0)
      translate (Vector3 (fromIntegral x) (-fromIntegral y) (0 :: GLfloat))
    moveCursorBack :: Float -> Int -> Int -> IO ()
    moveCursorBack asc x y = do
      translate (Vector3 0 asc 0)
      translate (Vector3 (-fromIntegral x) (fromIntegral y) (0 :: GLfloat))
    getFontAndLayout :: Int -> IO (Graphics.Rendering.FTGL.Font, Layout)
    getFontAndLayout fontSize = do
      font <- getFont fontPath fontSize -- stop doing that each time (I think)
      layout <- createSimpleLayout
      setLayoutFont layout font
      setLayoutLineLength layout (fromIntegral w)
      return (font, layout)

drawBox :: Box -> IO ()
drawBox (Box (w, h) (x, y)) = do
  style <- get currentStyle
  mapM_ Graphics.UI.GLUT.color $ Parsers.ElementStyle.background style
  rect
    (Vertex2 (fromIntegral x :: GLfloat) (-fromIntegral (y + h)))
    (Vertex2 (fromIntegral (x + w)) (-fromIntegral y))

getElementStyle :: DisplayTree -> ElementStyle
getElementStyle (DisplayLeaf _ elementStyle _) = elementStyle
getElementStyle (DisplayNode _ elementStyle _) = elementStyle

drawDisplayTree :: FontPath -> DisplayTree -> IO ()
drawDisplayTree fontPath tree = withStyle (getElementStyle tree) (drawDisplayTree' tree)
  where
    drawDisplayTree' (DisplayLeaf box _ "") = drawBox box
    drawDisplayTree' (DisplayLeaf box@(Box dim pos) _ text) = drawBox box >> showText fontPath pos dim text
    drawDisplayTree' (DisplayNode box elementStyle []) = drawBox box
    drawDisplayTree' (DisplayNode box elementStyle children) = drawBox box >> forM_ children (drawDisplayTree fontPath)

maybeToDefautPage :: FontPath -> String -> String -> DisplayTree
maybeToDefautPage fontPath html css = fromMaybe errorTree displayTree
  where
    styleSheet :: StyleSheet
    styleSheet = snd (fromJust (runParser parseStyleSheet css))
    styleMap :: StyleMap
    styleMap = cascadeSheets styleSheet

    displayTree :: Maybe DisplayTree
    displayTree = parseDisplay' html
    errorTree :: DisplayTree
    errorTree = fromJust $ parseDisplay' errorPage
      where
        errorPage :: String
        errorPage = "<div>Error 404</div>"

    parseDisplay' :: String -> Maybe DisplayTree
    parseDisplay' file = snd <$> runParser (parseDisplay styleMap fontPath (0, 0) windowInitWidth) file

setViewWithScroll :: IO ()
setViewWithScroll = do
  (Size w h) <- get windowSize
  (ScrollState s pH) <- get currentScrollState

  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (fromIntegral w) (-fromIntegral (h + fromIntegral s)) (-fromIntegral s)
  matrixMode $= Modelview 0

onReshape :: ReshapeCallback
onReshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  setViewWithScroll

onScroll :: Int -> MouseWheelCallback
onScroll pageH _ dir _ = do
  scrollState <- get currentScrollState
  currentScrollState $= scrollALittle scrollState

  setViewWithScroll

  postRedisplay Nothing -- replace this pls, only update matrix (if possible)
  where
    speed = 50
    scrollALittle (ScrollState s _) = ScrollState (-dir * speed + s) pageH

onDisplay :: FontPath -> DisplayTree -> DisplayCallback
onDisplay fontPath displayTree = do
  clear [ColorBuffer]
  drawDisplayTree fontPath displayTree
  flush

setCallbacks :: HTMLFile -> CSSFile -> FontPath -> IO ()
setCallbacks htmlFile cssFile fontPath = do
  let displayTree = maybeToDefautPage fontPath htmlFile cssFile

  reshapeCallback $= Just onReshape
  displayCallback $= onDisplay fontPath displayTree
  mouseWheelCallback $= Just (onScroll (getPageHeight displayTree))
  where
    getPageHeight (DisplayNode (Box (_, h) _) _ _) = h
    getPageHeight (DisplayLeaf (Box (_, h) _) _ _) = h
