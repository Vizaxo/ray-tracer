module Ray where

import Data.Array
import Data.Glome.Vec as V
import Graphics.Image hiding (Array, map)
import Debug.Trace
import Data.Maybe
import Data.List hiding (intersect)
import Data.Ord
import System.Random

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
  = [(ro `vadd` (rd `vscale` t), vinvert n) --TODO: calculate whether to invert this based on which face was hit
    | let denom = vdot n rd
    , denom /= 0
    , let t = (vdot n (ro `vsub` d)) / denom
    , t > 0.00001
    ]

rayTrace :: RandomGen g => World -> Ray -> g -> Int -> Pixel RGB Double
rayTrace w ray rand 0 = colour black
rayTrace w ray rand limit = toColor $ concat $ (removeEmpties . (\o -> (intersect ray (shape o), material o))) <$> (objects w)
  where toColor :: [(Hit, Material)] -> Colour
        toColor [] = colour (sky w)
        toColor vs = mkColour $ head $
          sortBy (comparing ((\v -> vlen (origin cam `vsub` v)) . fst . fst)) $ vs
        mkColour ((hitPos, hitNorm), mat)
          = (colour mat + rayTrace w (Ray hitPos newRayDir) rand' (limit - 1))
          where
            reflectedRay = (vnorm (dir ray `vsub` hitNorm `vscale`
                                   (2 * (dir ray `vdot` hitNorm))))
            (newRayDir, rand') = let
              (x, r1) = random rand
              (y, r2) = random r1
              (z, r3) = random r2
              in (vnorm (vmap2 (lerp (specular mat)) (hitNorm `vadd` (Vec x y z)) reflectedRay), r3)
        removeEmpties ([], c) = []
        removeEmpties (xs, c) = (,c) <$> xs
        cam = camera w

lerp :: Fractional n => Double -> n -> n -> n
lerp l a b = (realToFrac $ 1 - l) * a + realToFrac l * b

--Calculate the ray to project onto the film
film :: ImageProperties -> Ray -> Int -> Int -> Ray
film img cam i j = Ray (origin cam) direction
  where direction = (vnorm (dir cam)) `vadd` (Vec (fromIntegral i / fromIntegral (width img) - (0.5)) 0 (fromIntegral j / fromIntegral (height img) - (0.5)))

multipleRays :: RandomGen g => Int -> World -> Ray -> g -> Pixel RGB Double
multipleRays count world ray rand = avg $ take count $ fmap (\r -> rayTrace world ray r 10) $ mkRands rand
  where
    avg :: [Pixel RGB Double] -> Pixel RGB Double
    avg = foldr (+) 0 . fmap (/ (realToFrac count))

render :: RandomGen g => ImageProperties -> World -> g -> Image VU RGB Double
render img world rand = makeImage (width img, height img)
  (\(i,j) -> multipleRays 16 world (film img (camera world) j i) (rands ! (i,j)))
  where
    rands = splitMany rand (width img) (height img)

splitMany :: RandomGen g => g -> Int -> Int -> Array (Int, Int) g
splitMany rand x y = listArray ((0,0), (x,y)) (mkRands rand)

mkRands :: RandomGen g => g -> [g]
mkRands = unfoldr (pure . split)

testWorld :: World
testWorld = World
  { camera = Ray camPos (vnorm (vzero `vsub` camPos))
  , objects = [ Object (Plane (vnorm (Vec 0.1 0.0 1)) (Vec 0 0 0)) cherryRed
              , Object (Plane (vnorm (Vec (-0.1) 0.0 1)) (Vec 0 0 (-0.1))) dullGreen
              , Object (Sphere (Vec 0 (-2) 1.5) 1) mirror
              , Object (Sphere (Vec 1 (-6) 1) 0.3) redLight
              ]
  , sky = mkColour 0.2 0.3 0.3
  }
  where
    camPos = Vec 0 (-10) 1

skyBlue = mkColour 0.2 0.4 0.7
cherryRed = mkColour 0.8 0.2 0.0
dullGreen = mkColour 0.1 0.8 0.0
grey = mkColour 0.4 0.4 0.4
pink = mkColour 0.6 0.2 0.2
black = mkColour 0.0 0.0 0.0
mirror = Material { colour = PixelRGB 0.1 0.5 0.0, specular = 1 }
mkColour r g b = Material { colour = PixelRGB r g b, specular = 0.1 }

smallImage :: ImageProperties
smallImage = ImageProperties { width = 720, height = 720 }

testImage :: RandomGen g => g -> Image VU RGB Double
testImage rand = render smallImage testWorld rand

main :: IO ()
main = do rand <- newStdGen
          writeImage "test.png" (testImage rand)
          return ()
