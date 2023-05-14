module Display where

import BoxTree
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
import ParserUtils.Parser
import System.Random.MWC (Gen, create)

unif :: RVar Float
unif = Data.Random.uniform 0 (1 :: Float)

randomColor3 :: StatefulGen g m => g -> m (Color3 GLfloat)
randomColor3 mwc = do
  x <- sampleFrom mwc unif
  y <- sampleFrom mwc unif
  z <- sampleFrom mwc unif
  return $ Color3 (x :: GLfloat) (y :: GLfloat) (z :: GLfloat)

onReshape :: ReshapeCallback
onReshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (fromIntegral w) (-fromIntegral h) 0
  matrixMode $= Modelview 0

windowWidth :: Int
windowWidth = 1200

windowHeight :: Int
windowHeight = 800

initializeGL :: IO ()
initializeGL = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered, RGBMode]
  initialWindowSize $= Size 1200 800
  _ <- createWindow progName
  clearColor $= Color4 1 1 1 1.0

fontPath :: String
fontPath = "app/font/Roboto/Roboto-Regular.ttf"

getFont :: Int -> IO Graphics.Rendering.FTGL.Font
getFont size = do
  font <- createTextureFont fontPath
  setFontFaceSize font size 10
  return font

showText :: Graphics.Rendering.FTGL.Font -> CornerPosition -> Dimension -> String -> IO ()
showText font (x, y) (w, h) text = do
  layout <- createSimpleLayout
  setLayoutFont layout font
  setLayoutLineLength layout (fromIntegral w)
  let asc = getFontAscender font
  translatef (Vector3 0 (-asc) 0)
  translatef (Vector3 (fromIntegral x) (-fromIntegral (y)) 0)
  col <- get currentColor
  color3f (Color3 0 0 0)

  renderLayout layout text
  translatef (Vector3 0 asc 0)
  translatef (Vector3 (-fromIntegral x) (fromIntegral (y)) 0)
  color col
  where
    color3f = color :: Color3 GLfloat -> IO ()
    scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
    translatef = translate :: Vector3 GLfloat -> IO ()

drawBox :: Color3 GLfloat -> Box -> IO ()
drawBox col (Box (w, h) (x, y)) = do
  cInit <- get currentColor
  color3f col
  rect
    (Vertex2 (fromIntegral x :: GLfloat) (-fromIntegral y))
    (Vertex2 (fromIntegral (x + w)) (-fromIntegral (y + h)))
  color cInit
  where
    color3f = color :: Color3 GLfloat -> IO ()

drawDisplayTree :: Graphics.Rendering.FTGL.Font -> Gen RealWorld -> DisplayTree -> IO ()
drawDisplayTree font mwc (DisplayLeaf box "") = do
  col <- randomColor3 mwc
  drawBox col box
drawDisplayTree font mwc (DisplayLeaf box@(Box dim pos) text) = do
  col <- randomColor3 mwc
  drawBox col box
  showText font pos dim text
drawDisplayTree font mwc (DisplayNode box []) = do
  col <- randomColor3 mwc
  drawBox col box
drawDisplayTree font mwc (DisplayNode box children) = do
  col <- randomColor3 mwc
  drawBox col box
  forM_ children $ drawDisplayTree font mwc

maybeToDefautPage :: Graphics.Rendering.FTGL.Font -> String -> DisplayTree
maybeToDefautPage font page =
  case runParser (parseDisplay font (0, 0) windowWidth) page of
    (Just ("", res)) -> res
    Nothing -> case runParser (parseDisplay font (0, 0) windowWidth) "<div>Error 404</div>" of
      Just ("", res) -> res

mainDisplay :: IO ()
mainDisplay = do
  page <- readFile "file.html"

  initializeGL
  font <- getFont 16

  mwc <- create

  reshapeCallback $= Just onReshape

  let displays = maybeToDefautPage font page

  ($=) displayCallback $ clear [ColorBuffer] >> drawDisplayTree font mwc displays >> flush

  mainLoop

  destroyFont font