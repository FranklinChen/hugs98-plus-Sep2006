module Main(main) where

import Graphics.HGL

main :: IO ()
main = runGraphics $ do
  w <- openWindow "Hello World Window" (300, 300)
  drawInWindow w (text (100, 100) "Hello")
  drawInWindow w (text (100, 200) "World")
  getKey w
  closeWindow w
