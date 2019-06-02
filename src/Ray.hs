module Ray where

import Data.Array
import Data.Glome.Vec as V
import Graphics.Image hiding (Array, map)
import Debug.Trace
import Data.Maybe
import Data.List hiding (intersect)
import Data.Ord
import System.Random

tau :: Floating a => a
tau = 2 * pi

type Colour = Pixel RGB Double

data Material = Material
  { diffuseColour :: Colour
  , emissionColour :: Colour
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
rayTrace w ray rand 0 = diffuseColour (sky w)
rayTrace w ray rand limit = toColor $ concat $ (removeEmpties . (\o -> (intersect ray (shape o), material o))) <$> (objects w)
  where toColor :: [(Hit, Material)] -> Colour
        toColor [] = diffuseColour (sky w)
        toColor vs = mkColour $ head $
          sortBy (comparing ((\v -> vlen (origin cam `vsub` v)) . fst . fst)) $ vs
        mkColour ((hitPos, hitNorm), mat)
          = (emissionColour mat + diffuseColour mat * rayTrace w (Ray hitPos newRayDir) rand' (limit - 1))
          where
            reflectedRay = (vnorm (dir ray `vsub` hitNorm `vscale`
                                   (2 * (dir ray `vdot` hitNorm))))
            (newRayDir, rand') = let
              (r1, r2) = split rand
              in (vnorm (vmap2 (lerp (specular mat)) (sampleHemisphere r1 hitNorm) reflectedRay), r2) --TODO: uniform vector lerp?
        removeEmpties ([], c) = []
        removeEmpties (xs, c) = (,c) <$> xs
        cam = camera w

-- Uniform sampling of points of a hemisphere with its pole in the
-- direction of v
sampleHemisphere :: RandomGen g => g -> Vec -> Vec
sampleHemisphere rand v =
  let u = sampleSphere rand
  in if v `vdot` u >= 0 then u else (vinvert u)

sampleSphere :: RandomGen g => g -> Vec
sampleSphere rand = let
  (u1, rand') = random rand
  r = sqrt (1 - u1*u1)
  (u2, _) = random rand'
  phi = tau * u2
  in Vec (cos phi * r) (sin phi * r) (2 * u1)

lerp :: Fractional n => Double -> n -> n -> n
lerp l a b = (realToFrac $ 1 - l) * a + realToFrac l * b

--Calculate the ray to project onto the film
film :: ImageProperties -> Ray -> Double -> Double -> Ray
film img cam i j = Ray (origin cam) direction
  --TODO: convert film to camera's coordinate system. Currently the film is on the global xz plane.
  where direction = (vnorm (dir cam)) `vadd` (Vec (i / fromIntegral (width img) - 0.5) 0 (j / fromIntegral (height img) - 0.5))

jitter :: RandomGen g => g -> Int -> Double
jitter rand n = (r - 0.5) + realToFrac n
  where r = fst $ random rand

multipleRays :: RandomGen g => Int -> World -> ImageProperties -> Int -> Int -> g -> Pixel RGB Double
multipleRays count world img i j rand
  = expAvg $ take count $ fmap (singleRay . split3) $ mkRands rand
  where
    singleRay = (\(r1, r2, r3) -> rayTrace world (film img (camera world) (jitter r1 i) (jitter r2 j)) r3 4)

    --TODO: properly manage linear/logarithmic lighting
    expAvg :: [Pixel RGB Double] -> Pixel RGB Double
    expAvg = brighten . (**(1/lightPower)) . foldr (+) 0 . fmap (/ (realToFrac count)) . fmap (**lightPower)

    lightPower :: Pixel RGB Double
    lightPower = 2
    brighten = (**0.25)

render :: RandomGen g => ImageProperties -> World -> g -> Image VU RGB Double
render img world rand = makeImage (height img, width img)
  (\(j,i) -> multipleRays 32 world img i j (rands ! (i,j)))
  where
    rands = splitMany rand (width img) (height img)

split3 :: RandomGen g => g -> (g, g, g)
split3 (split -> (r1, r2)) = (r2, r3, r4) where
  (r3, r4) = split r1

splitMany :: RandomGen g => g -> Int -> Int -> Array (Int, Int) g
splitMany rand x y = listArray ((0,0), (x,y)) (mkRands rand)

mkRands :: RandomGen g => g -> [g]
mkRands = unfoldr (pure . split)

testWorld :: World
testWorld = World
  { camera = Ray camPos (vnorm (vinvert camPos))
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
cherryRed = mkColour 0.9 0.3 0.2
dullGreen = mkColour 0.3 0.8 0.4
grey = mkColour 0.4 0.4 0.4
pink = mkColour 0.9 0.3 0.4
black = mkColour 0.0 0.0 0.0
mirror = Material { diffuseColour = PixelRGB 0.97 0.97 1, emissionColour = 0, specular = 0.99 }
mkColour r g b = Material { diffuseColour = PixelRGB r g b, emissionColour = 0, specular = 0.0 }
redLight = Material { diffuseColour = 0, emissionColour = PixelRGB 1 0.3 0.1, specular = 0.1 }

smallImage :: ImageProperties
smallImage = ImageProperties { width = 720, height = 720 }

testImage :: RandomGen g => g -> Image VU RGB Double
testImage rand = render smallImage testWorld rand

main :: IO ()
main = do rand <- newStdGen
          writeImage "test.png" (testImage rand)
          return ()
