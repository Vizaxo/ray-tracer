module Materials where

import Graphics.Image

type Colour = Pixel RGB Double

type RefractiveIndex = Double

data Material = Material
  { diffuseColour :: Colour
  , emissionColour :: Colour
  , specular :: Double -- 0 = pure diffuse, 1 = pure specular
  , transparency :: Double -- 0 = opaque, 1 = transparent
  , refractiveIndex :: RefractiveIndex
  }
  deriving Show

defaultMaterial :: Material
defaultMaterial = Material
  { diffuseColour = black
  , emissionColour = black
  , specular = 0
  , transparency = 0
  , refractiveIndex = 1
  }

black :: Colour
black = 0

mkColour r g b = defaultMaterial {diffuseColour = PixelRGB r g b}
mkLight r g b = defaultMaterial {emissionColour = PixelRGB r g b}

blackDiffuse = mkColour 0 0 0
skyBlue = mkColour 0.2 0.4 0.7
cherryRed = mkColour 0.9 0.3 0.2
dullGreen = mkColour 0.3 0.8 0.4
paleBlue = mkColour 0.3 0.6 1
grey = mkColour 0.4 0.4 0.4
pink = mkColour 0.9 0.3 0.4
red = mkColour 1.0 0.2 0.2
brown = mkColour 0.6 0.4 0.3
mirror = defaultMaterial {diffuseColour = PixelRGB 0.97 0.97 1
                         , specular = 1}
glass = defaultMaterial {diffuseColour = PixelRGB 1 1 1
                        , specular = 1
                        , transparency = 0.95
                        , refractiveIndex = 1.5 }
redLight = mkLight 1 0.3 0.1
blueLight = mkLight 0.002 0.002 0.01
greenLight = mkLight 0.002 0.01 0.001
