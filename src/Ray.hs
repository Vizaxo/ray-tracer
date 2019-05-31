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

intersect (Ray rayOrigin rd) (Sphere centre r) =
  [ ro `vadd` (rd `vscale` t)
  | let ro = rayOrigin `vsub` centre
        a = rd `vdot` rd
        b = 2 * (rd `vdot` ro)
        c = (ro `vdot` ro) - r^2
        denom = 2 * a
        root = sqrt (b^2 - 4*a*c)
  , plusMinus <- [(-1), 1]
  , let t = (-b + (plusMinus * root)) / denom
  , t > 0
  ]
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
        toColor vs = snd $ head $ sortBy (comparing fst) $ map (\(v, c) -> (vlen (origin cam `vsub` v), c)) vs --List of all intersections. Pick the closest one.
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
  { camera = Ray camPos (vnorm (origin `vsub` camPos))
  , objects = [ Object (Plane (vnorm (Vec 0.1 0.0 1)) (Vec 0 0 0)) cherryRed
              , Object (Plane (vnorm (Vec (-0.1) 0.0 1)) (Vec 0 0 (-0.1))) dullGreen
              , Object (Sphere (Vec 0 (-2) 1.5) 1) black
              , Object (Sphere (Vec 1 (-6) 1) 0.3) grey
              ]
  , sky = skyBlue
  }
  where
    camPos = Vec 0 (-10) 1
    skyBlue = Material { colour = PixelRGB 0.2 0.4 0.7 }
    cherryRed = Material { colour = PixelRGB 0.8 0.2 0.0 }
    dullGreen = Material { colour = PixelRGB 0.1 0.8 0.0 }
    grey = Material { colour = PixelRGB 0.4 0.4 0.4 }
    black = Material { colour = PixelRGB 0.0 0.0 0.0 }
    origin = Vec 0 0 0

smallImage :: ImageProperties
smallImage = ImageProperties { width = 240, height = 240 }

testImage :: Image VU RGB Double
testImage = render smallImage testWorld

main :: IO ()
main = do writeImage "test.png" testImage
          return ()
