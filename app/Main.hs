module Main where

import Display (initializeGL, setCallbacks)
import Graphics.UI.GLUT (mainLoop)
import Parsers.JSParser (mainJS)

mainFinal :: IO ()
mainFinal = do
  htmlFile <- readFile htmlPath
  cssFile <- readFile cssPath
  initializeGL
  setCallbacks htmlFile cssFile fontPath
  mainLoop
  where
    htmlPath :: String
    htmlPath = "testFiles/file.html"
    cssPath :: String
    cssPath = "testFiles/style.css"
    fontPath :: String
    fontPath = "testFiles/font/Roboto/Roboto-Regular.ttf"

main :: IO ()
main = mainJS