module Main where

import Image
import Ray
import Scene

testImg :: ImageProperties
testImg = ImageProperties
  { width = 300
  , height = 300
  , raysPerPixel = 1024
  , maxBounces = 10
  , flatColours = False
  }

main :: IO ()
main = writeSceneImage testImg cornellBox "image.png"
