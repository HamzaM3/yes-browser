module Display where

import BoxTree
  ( Box (..),
    CornerPosition,
    Dimension,
    DisplayTree (..),
    FontPath,
    ShallowTree,
    htmlToShallow,
    parseDisplay,
    shallowToDisplay,
  )
import Control.Monad (forM_, when)
import Data.Maybe (fromJust, fromMaybe)
import Data.StateVar
  ( HasGetter (get),
    HasSetter (($=)),
  )
import GHC.IO.Unsafe (unsafePerformIO)
import GlobalStates.GlobalDOM (currentFontPath, currentHTML, currentStyle, currentStyleMap, withStyle)
import GlobalStates.GlobalEventLoop (combineStyleChange, currentStyleChange)
import GlobalStates.GlobalScroll (ScrollState (ScrollState, scrollPosition), currentScrollState)
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
    KeyState (Down),
    MatrixComponent (translate),
    MatrixMode (Modelview, Projection),
    MouseButton (LeftButton),
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
    mouseCallback,
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
import Parsers.ElementStyle (ElementStyle (background, color, fontSize), emptyStyle)
import Parsers.HTMLParser (HTMLDOM, parseHTMLPage)
import Parsers.JSParser (parseScript)
import Parsers.ParserUtils.Parser (Parser (runParser))

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

type JSFile = String

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

getHTMLFromFile :: String -> HTMLDOM
getHTMLFromFile = snd . fromJust . runParser parseHTMLPage

getStyleMapFromFile :: String -> StyleMap
getStyleMapFromFile = cascadeSheets . snd . fromJust . runParser parseStyleSheet

maybeToDefautPage :: FontPath -> HTMLDOM -> StyleMap -> DisplayTree
maybeToDefautPage fontPath htmlDOM styleMap = displayTree
  where
    shallowTree :: ShallowTree
    shallowTree = htmlToShallow styleMap htmlDOM

    displayTree :: DisplayTree
    displayTree =
      shallowToDisplay
        emptyStyle
        fontPath
        (0, 0)
        windowInitWidth
        shallowTree

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

onDisplay :: FontPath -> DisplayCallback
onDisplay fontPath = do
  clear [ColorBuffer]
  displayTree <- calculateDisplayTree
  drawDisplayTree fontPath displayTree
  flush

getCurrentDOMData :: IO (FontPath, HTMLDOM, StyleMap)
getCurrentDOMData = do
  fontPath <- get currentFontPath
  htmlDOM <- get currentHTML
  styleMap <- get currentStyleMap
  return (fontPath, htmlDOM, styleMap)

calculateDisplayTree :: IO DisplayTree
calculateDisplayTree = do
  (fontPath, htmlDOM, styleMap) <- getCurrentDOMData
  let displayTree = maybeToDefautPage fontPath htmlDOM styleMap
  return displayTree

onClick :: IO ()
onClick = do
  styleChange <- get currentStyleChange
  styleMap <- get currentStyleMap
  currentStyleMap $= styleChange styleMap
  postRedisplay Nothing

onMouseEvent :: MouseButton -> KeyState -> Position -> IO ()
onMouseEvent LeftButton Down _ = onClick
onMouseEvent _ _ _ = return ()

setCallbacks :: HTMLFile -> CSSFile -> JSFile -> FontPath -> IO ()
setCallbacks htmlFile cssFile jsFile fontPath = do
  currentStyleMap $= getStyleMapFromFile cssFile
  currentHTML $= getHTMLFromFile htmlFile
  currentFontPath $= fontPath

  (fontPath, htmlDOM, styleMap) <- getCurrentDOMData

  let displayTree = maybeToDefautPage fontPath htmlDOM styleMap

  -- lazy is real
  let clickEvents = snd $ fromJust $ runParser parseScript jsFile
  let backgroundChangeFunc = combineStyleChange clickEvents

  currentStyleChange $= backgroundChangeFunc

  reshapeCallback $= Just onReshape
  displayCallback $= onDisplay fontPath
  mouseWheelCallback $= Just (onScroll (getPageHeight displayTree))
  mouseCallback $= Just onMouseEvent
  where
    getPageHeight (DisplayNode (Box (_, h) _) _ _) = h
    getPageHeight (DisplayLeaf (Box (_, h) _) _ _) = h
    shallowToDisplay' :: ShallowTree -> DisplayTree
    shallowToDisplay' = shallowToDisplay emptyStyle fontPath (0, 0) windowInitWidth
