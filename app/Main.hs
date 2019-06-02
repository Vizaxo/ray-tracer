module Main where

import Data.Glome.Vec as V
import Graphics.Image hiding (Array, map)
import System.Random

import Ray

testWorld :: World
testWorld = World
  { camera = Ray camPos (vnorm (vinvert camPos))
  , objects = [ Object (Plane (vnorm (Vec 0.1 0.0 1)) (-2)) cherryRed
              , Object (Plane (vnorm (Vec (-0.1) 0.0 1)) (-2)) dullGreen
              , Object (Sphere (Vec (-1) (-2) 0) 1) mirror
              , Object (Sphere (Vec 0 (-4) 1) 0.6) mirror
              , Object (Sphere (Vec (-1) (-4) (-1.5)) 1) mirror
              , Object (Sphere (Vec 2 (-2) 0) 1) pink
              , Object (Sphere (Vec 1.3 0 (-0.5)) 1.2) redLight
              , Object (Sphere (Vec 3 (-5) (-1)) 1.2) blueLight
              , Object (Sphere (Vec (-2) (-3) (4)) 0.6) greenLight
              , Object (Sphere (Vec 0 (-5) 0) 1) greenGlass
              ]
  , sky = mkColour 0.01 0.02 0.01
  }
  where
    camPos = Vec 0 (-10) 1

skyBlue = mkColour 0.2 0.4 0.7
cherryRed = mkColour 0.9 0.3 0.2
dullGreen = mkColour 0.3 0.8 0.4
grey = mkColour 0.4 0.4 0.4
pink = mkColour 0.9 0.3 0.4
black = mkColour 0.0 0.0 0.0
mirror = Material { diffuseColour = PixelRGB 0.97 0.97 1, emissionColour = 0, specular = 0.999, transparency = 0 }
mkColour r g b = Material { diffuseColour = PixelRGB r g b, emissionColour = 0, specular = 0.0, transparency = 0 }
redLight = Material { diffuseColour = 0, emissionColour = PixelRGB 1 0.3 0.1, specular = 0.1, transparency = 0 }
blueLight = Material { diffuseColour = 0, emissionColour = PixelRGB 0.2 0.3 1, specular = 0.1, transparency = 0 }
greenLight = Material { diffuseColour = 0, emissionColour = PixelRGB 0.1 0.4 0.1, specular = 0.1, transparency = 0 }
greenGlass = Material { diffuseColour = PixelRGB 0.95 1 0.95, emissionColour = 0, specular = 0.99, transparency = 0.9 }

smallImage :: ImageProperties
smallImage = ImageProperties
  { width = 720
  , height = 720
  , raysPerPixel = 256
  , maxBounces = 10 }

testImage :: RandomGen g => g -> Image VU RGB Double
testImage rand = render smallImage testWorld rand

main :: IO ()
main = do rand <- newStdGen
          writeImage "test.png" (testImage rand)
          return ()
