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

defaultMaterial :: Material
defaultMaterial = Material
  { diffuseColour = black
  , emissionColour = black
  , specular = 0
  , transparency = 0
  }

skyBlue = mkColour 0.2 0.4 0.7
cherryRed = mkColour 0.9 0.3 0.2
dullGreen = mkColour 0.3 0.8 0.4
grey = mkColour 0.4 0.4 0.4
pink = mkColour 0.9 0.3 0.4
mirror = defaultMaterial {diffuseColour = PixelRGB 0.97 0.97 1, specular = 1}
mkColour r g b = defaultMaterial {diffuseColour = PixelRGB r g b}
redLight = defaultMaterial {emissionColour = PixelRGB 1 0.3 0.1}
blueLight = defaultMaterial {emissionColour = PixelRGB 0.2 0.3 1}
greenLight = defaultMaterial {emissionColour = PixelRGB 0.1 0.4 0.1}
greenGlass = defaultMaterial {diffuseColour = PixelRGB 0.95 1 0.95, specular = 0.99, transparency = 0.9 }

hqImage :: ImageProperties
hqImage = ImageProperties
  { width = 720
  , height = 720
  , raysPerPixel = 256
  , maxBounces = 10 }

testImage :: ImageProperties
testImage = ImageProperties
  { width = 150
  , height = 150
  , raysPerPixel = 8
  , maxBounces = 4 }

renderedImage :: RandomGen g => g -> Image VU RGB Double
renderedImage rand = render testImage testWorld rand

main :: IO ()
main = do rand <- newStdGen
          writeImage "test.png" (renderedImage rand)
          return ()
