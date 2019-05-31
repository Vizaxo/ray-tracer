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
  }
  deriving Show

data Shape
  = Sphere {centre :: Vec, radius :: Float}
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

intersect ray (Sphere _ _) = undefined
intersect (Ray ro rd) (Plane n d) = [ro `vadd` (rd `vscale` t)
                                    | let denom = vdot n rd
                                    , denom /= 0
                                    , let t = (vdot n (ro `vsub` d)) / denom
                                    , t > 0
                                    ]

rayTrace :: ImageProperties -> World -> Int -> Int -> Pixel RGB Double
rayTrace img w i j = toColor $ concat $ (removeEmpties . (\o -> (intersect (film img cam i j) (shape o), colour (material o)))) <$> (objects w)
  where toColor :: [(Vec, Colour)] -> Colour
        toColor [] = colour (sky w)
        toColor vs = (\(dist, c) -> PixelRGB (dist / 100) 0.5 0.5) $ head $ sortBy (comparing fst) $ map (\(v, c) -> (vlen (origin cam `vsub` v), c)) vs --List of all intersections. Pick the closest one.
        removeEmpties ([], c) = []
        removeEmpties (xs, c) = (,c) <$> xs
        cam = camera w

--Calculate the ray to project onto the film
film :: ImageProperties -> Ray -> Int -> Int -> Ray
film img cam i j = Ray (origin cam) direction
  where direction = (vnorm (dir cam)) `vadd` (Vec (fromIntegral i / fromIntegral (width img) - (0.5)) 0 (fromIntegral j / fromIntegral (height img) - (0.5)))

render :: ImageProperties -> World -> Image VU RGB Double
render img world = makeImage (width img, height img) (\(i,j) -> rayTrace img world j i)

testWorld :: World
testWorld = World
  { camera = Ray (Vec 0 10 1) (Vec 1 (-0.1) (-0.1))
  , objects = [ Object (Plane (Vec 0.002 0.0 0.1) (Vec 0 (-00) 0)) cherryRed
              , Object (Plane (Vec 0.3 (0.5) 1) (Vec 0 (-10) 0)) dullGreen
              , Object (Plane (Vec 0.03 0.60 1) (Vec 0 (-00) 0)) grey
              ]
  , sky = skyBlue
  }
  where
    skyBlue = Material { colour = PixelRGB 0.2 0.4 0.7 }
    cherryRed = Material { colour = PixelRGB 0.8 0.2 0.0 }
    dullGreen = Material { colour = PixelRGB 0.1 0.8 0.0 }
    grey = Material { colour = PixelRGB 0.4 0.4 0.4 }

smallImage :: ImageProperties
smallImage = ImageProperties { width = 480, height = 480 }

testImage :: Image VU RGB Double
testImage = render smallImage testWorld

main :: IO ()
main = do writeImage "test.png" testImage
          return ()
