module Main where

import Display (initializeGL, setCallbacks)
import Graphics.UI.GLUT (mainLoop)

main :: IO ()
main = do
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
