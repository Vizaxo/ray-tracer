module Main where

import Data.Glome.Vec as V
import Graphics.Image as I
import System.Random

import Ray

testWorld :: World
testWorld = World
  { camera = Ray camPos (vnorm (vinvert camPos))
  , objects = [ Object (Plane (vnorm (Vec 0.1 0.0 1)) (-2)) cherryRed
              , Object (Plane (vnorm (Vec (-0.1) 0.0 1)) (-2)) dullGreen
              --, Object (Plane (vnorm (vinvert vy)) (-10)) paleBlue
              , Object (Sphere (Vec (-1) (-2) 0) 1) mirror
              --, Object (Sphere (Vec 0 (-4) 1) 0.6) mirror
              , Object (Sphere (Vec (-1) (-4) (-1.5)) 1) mirror
              , Object (Sphere (Vec 2 (-2) 0) 1) pink
              , Object (Sphere (Vec 0 10 2) 4) paleBlue
              , Object (Sphere (Vec (-4) (-1.5) (-0.5)) 1) brown
              --, Object (Sphere (Vec 1.3 0 (-0.5)) 1.2) redLight
              , Object (Sphere (Vec 3 (-5) (-1)) 1.2) blueLight
              , Object (Sphere (Vec (-2) (-3) (4)) 0.6) greenLight
              , Object (Sphere (Vec (0.3) (-5) 0) 1) glass
              --, Object (Sphere (Vec 1 (-6.5) 0.1) 0.8) glass
              ]
  , sky = mkSky 0.005
  }
  where
    camPos = Vec 0 (-10) 1
    mkSky factor = mkColour (factor * 1) (factor * 1) (factor * 1.5)

defaultMaterial :: Material
defaultMaterial = Material
  { diffuseColour = black
  , emissionColour = black
  , specular = 0
  , transparency = 0
  , refractiveIndex = 1
  }

skyBlue = mkColour 0.2 0.4 0.7
cherryRed = mkColour 0.9 0.3 0.2
dullGreen = mkColour 0.3 0.8 0.4
paleBlue = mkColour 0.3 0.6 1
grey = mkColour 0.4 0.4 0.4
pink = mkColour 0.9 0.3 0.4
red = mkColour 1.0 0.2 0.2
brown = mkColour 0.6 0.4 0.3
mirror = defaultMaterial {diffuseColour = PixelRGB 0.97 0.97 1, specular = 1}
mkColour r g b = defaultMaterial {diffuseColour = PixelRGB r g b}
redLight = defaultMaterial {emissionColour = PixelRGB 1 0.3 0.1}
blueLight = defaultMaterial {emissionColour = PixelRGB 0.002 0.002 0.01}
greenLight = defaultMaterial {emissionColour = PixelRGB 0.002 0.01 0.001}
glass = defaultMaterial {diffuseColour = PixelRGB 1 1 1, specular = 0.99, transparency = 0.95, refractiveIndex = 1.1 }

hqImage :: ImageProperties
hqImage = ImageProperties
  { width = 720
  , height = 720
  , raysPerPixel = 256
  , maxBounces = 10
  , flatColours = False
  }

testImage :: ImageProperties
testImage = ImageProperties
  { width = 150
  , height = 150
  , raysPerPixel = 8
  , maxBounces = 4
  , flatColours = False
  }

renderedImage :: RandomGen g => g -> Image VU RGB Double
renderedImage rand = render testImage testWorld rand

tosRGB :: Image VU RGB Double -> Image VU RGB Double
tosRGB = I.map (fmap ((255*) . f)) where
  f :: Double -> Double
  f u | u <= 0.0031308 = 12.92 * u
      | otherwise      = 1.055 * u**(1/2.4) - 0.055

main :: IO ()
main = writeImage "image.png" =<< (tosRGB . renderedImage) <$> newStdGen
