module Ray where

import Data.Glome.Vec as V
import Graphics.Image hiding (map)
import Debug.Trace
import Data.Maybe
import Data.List hiding (intersect)
import Data.Ord

type Colour = Pixel RGB Double

data Material = Material
  { colour :: Colour
  , specular :: Double -- 0 = pure diffuse, 1 = pure specular
  }
  deriving Show

data Shape
  = Sphere {centre :: Vec, radius :: Flt}
  | Plane {normal :: Vec, point :: Vec}
  deriving Show

data Object = Object
  { shape :: Shape
  , material :: Material
  }
  deriving Show

data World = World
  { objects :: [Object]
  , camera :: Ray
  , sky :: Material
  }
  deriving Show

data ImageProperties = ImageProperties
  { width :: Int
  , height :: Int
  }
  deriving Show

type Hit = (Vec, Vec)

intersect :: Ray -> Shape -> [Hit]
intersect (Ray rayOrigin rd) (Sphere centre r)
  = [ (hitPos, vnorm (hitPos `vsub` centre))
    | let ro = rayOrigin `vsub` centre
          a = rd `vdot` rd
          b = 2 * (rd `vdot` ro)
          c = (ro `vdot` ro) - r^2
          denom = 2 * a
          root = sqrt (b^2 - 4*a*c)
    , plusMinus <- [(-1), 1]
    , let t = (-b + (plusMinus * root)) / denom
    , t > 0.00001
    , let hitPos = rayOrigin `vadd` (rd `vscale` t)
    ]
intersect (Ray ro rd) (Plane n d)
  = [(ro `vadd` (rd `vscale` t), n)
    | let denom = vdot n rd
    , denom /= 0
    , let t = (vdot n (ro `vsub` d)) / denom
    , t > 0.00001
    ]

rayTrace :: World -> Ray -> Int -> Pixel RGB Double
rayTrace w ray 0 = colour (sky w)
rayTrace w ray limit = toColor $ concat $ (removeEmpties . (\o -> (intersect ray (shape o), material o))) <$> (objects w)
  where toColor :: [(Hit, Material)] -> Colour
        toColor [] = colour (sky w)
        toColor vs = mkColour $ head $
          sortBy (comparing ((\v -> vlen (origin cam `vsub` v)) . fst . fst)) $ vs
        mkColour ((hitPos, hitNorm), mat)
          = lerp (specular mat)
            (colour mat)
            (colour mat + rayTrace w (Ray hitPos newRayDir) (limit - 1))
          where
            newRayDir = (vnorm (dir ray `vsub` hitNorm `vscale`
                                (2 * (dir ray `vdot` hitNorm))))
        removeEmpties ([], c) = []
        removeEmpties (xs, c) = (,c) <$> xs
        cam = camera w

lerp :: Fractional n => Double -> n -> n -> n
lerp l a b = (realToFrac $ 1 - l) * a + realToFrac l * b

--Calculate the ray to project onto the film
film :: ImageProperties -> Ray -> Int -> Int -> Ray
film img cam i j = Ray (origin cam) direction
  where direction = (vnorm (dir cam)) `vadd` (Vec (fromIntegral i / fromIntegral (width img) - (0.5)) 0 (fromIntegral j / fromIntegral (height img) - (0.5)))

render :: ImageProperties -> World -> Image VU RGB Double
render img world = makeImage (width img, height img) (\(i,j) -> rayTrace world (film img (camera world) j i) 20)

testWorld :: World
testWorld = World
  { camera = Ray camPos (vnorm (origin `vsub` camPos))
  , objects = [ Object (Plane (vnorm (Vec 0.1 0.0 1)) (Vec 0 0 0)) cherryRed
              , Object (Plane (vnorm (Vec (-0.1) 0.0 1)) (Vec 0 0 (-0.1))) dullGreen
              , Object (Plane (vnorm (Vec 0 (0.3) 1)) (Vec 0 0 100)) black
              , Object (Sphere (Vec 0 (-2) 1.5) 1) mirror
              , Object (Sphere (Vec 1 (-6) 1) 0.3) pink
              ]
  , sky = skyBlue
  }
  where
    camPos = Vec 0 (-10) 1
    skyBlue = mkColour 0.2 0.4 0.7
    cherryRed = mkColour 0.8 0.2 0.0
    dullGreen = mkColour 0.1 0.8 0.0
    grey = mkColour 0.4 0.4 0.4
    pink = mkColour 1 0.4 0.4
    black = mkColour 0.0 0.0 0.0
    mirror = Material { colour = PixelRGB 0.1 0.5 0.0, specular = 0.9 }
    origin = Vec 0 0 0
    mkColour r g b = Material { colour = PixelRGB r g b, specular = 0.1 }

smallImage :: ImageProperties
smallImage = ImageProperties { width = 720, height = 720 }

testImage :: Image VU RGB Double
testImage = render smallImage testWorld

main :: IO ()
main = do writeImage "test.png" testImage
          return ()
