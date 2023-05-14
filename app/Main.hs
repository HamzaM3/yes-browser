import CSSParser
import Data.List (genericLength)
import Display
import Foreign.C.String (castCharToCChar)
import Foreign.Marshal.Array (withArray)
import Graphics.UI.GLUT
  ( ClearBuffer (ColorBuffer),
    Color (color),
    Color3 (..),
    DataType (UnsignedByte),
    DisplayCallback,
    DisplayList (DisplayList),
    GLfloat,
    GLubyte,
    GeneratableObjectName (genObjectNames),
    HasSetter (($=)),
    Key (Char),
    KeyState (Down),
    KeyboardMouseCallback,
    ListMode (Compile),
    MatrixComponent (scale, translate),
    MatrixMode (Modelview, Projection),
    Position (Position),
    PrimitiveMode (Quads),
    Rect (rect),
    ReshapeCallback,
    ShadingModel (Flat),
    Size (Size),
    Vector3 (..),
    Vertex (vertex),
    Vertex2 (..),
    callLists,
    clear,
    defineList,
    flush,
    listBase,
    loadIdentity,
    matrixMode,
    ortho2D,
    postRedisplay,
    renderPrimitive,
    shadeModel,
    viewport,
  )
import HTMLParser
import System.Exit (ExitCode (ExitSuccess), exitWith)

main :: IO ()
main = mainCSS

-- myInit :: IO ()
-- myInit = do
--   shadeModel $= Flat

--   ok@(base@(DisplayList b) : _) <- genObjectNames 128
--   listBase $= base
--   let charToDisplayList c = DisplayList (b + fromIntegral (charToGLubyte c))
--   mapM_
--     (\(c, d) -> defineList (charToDisplayList c) Compile d)
--     [ ('A', drawLetter aData),
--       ('E', drawLetter eData),
--       ('P', drawLetter pData),
--       ('R', drawLetter rData),
--       ('S', drawLetter sData),
--       (' ', advance)
--     ]

-- printStrokedString :: String -> IO ()
-- printStrokedString s =
--   withArray (map charToGLubyte s) $
--     callLists (genericLength s) UnsignedByte
