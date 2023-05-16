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
    htmlPath = "file.html"
    cssPath :: String
    cssPath = "style.css"
    fontPath :: String
    fontPath = "app/font/Roboto/Roboto-Regular.ttf"
