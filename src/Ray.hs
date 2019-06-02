module Ray where

import Data.Array
import Data.Glome.Vec as V
import Graphics.Image hiding (Array, map)
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
  , transparency :: Double -- 0 = opaque, 1 = transparent
  }
  deriving Show

data Shape
  = Sphere {centre :: Vec, radius :: Flt}
  | Plane {normal :: Vec, point :: Flt}
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
  , raysPerPixel :: Int
  , maxBounces :: Int
  }
  deriving Show

-- Hit is a hit position, and the normal of the surface at that point
type Hit = (Vec, Vec)

intersect :: Ray -> Shape -> [Hit]
intersect (Ray rayOrigin rd) (Sphere centre r)
  = [ (hitPos, vnorm (hitPos `vsub` centre))
    | let ro = rayOrigin `vsub` centre
          -- a = rd `vdot` rd = 1 (rd is always a unit vector)
          b = 2 * (rd `vdot` ro)
          c = (ro `vdot` ro) - r^2
          root = (sqrt (b^2 - 4*c))
    , plusMinus <- [(-1), 1]
    , let t = (-b + (plusMinus * root)) / 2
    , t > 0.00001
    , let hitPos = rayOrigin `vadd` (rd `vscale` t)
    ]
intersect (Ray ro rd) (Plane n d)
  = [(ro `vadd` (rd `vscale` t), calcNormal)
    | let denom = vdot n rd
    , (denom /= 0)
    , let t = ((n `vdot` ro) + d / denom)
    , t > 0.00001
    ]
  where
    calcNormal = if rd `vdot` n <= 0 then n else vinvert n

rayTrace :: RandomGen g => World -> Ray -> g -> Int -> Pixel RGB Double
rayTrace w ray rand 0 = black
rayTrace w ray rand limit = toColor $ concat $ (removeEmpties . (\o -> (intersect ray (shape o), material o))) <$> (objects w)
  where toColor :: [(Hit, Material)] -> Colour
        toColor [] = diffuseColour (sky w)
        toColor vs = mkColour $ head $
          sortBy (comparing ((\v -> vlen (origin ray `vsub` v)) . fst . fst)) $ vs
        mkColour ((hitPos, hitNorm), mat)
          = emissionColour mat
          + diffuseColour mat * if p <= (transparency mat)
              then rayTrace w (Ray hitPos refractedRay) rand2 (limit - 1)
              else rayTrace w (Ray hitPos newRayDir) rand2 (limit - 1)
          where
            reflectedRay = (vnorm (dir ray `vsub` (hitNorm `vscale`
                                   (2 * (dir ray `vdot` hitNorm)))))
            refractedRay = dir ray
            (newRayDir, rand1) = let
              (r1, r2) = split rand
              in (vnorm (vmap2 (lerp (specular mat)) (sampleHemisphere r1 hitNorm) reflectedRay), r2) --TODO: uniform vector lerp?
            (p :: Double, rand2) = random rand1
        removeEmpties ([], c) = []
        removeEmpties (xs, c) = (,c) <$> xs

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
film img cam i j = Ray (origin cam) (vnorm direction)
  --TODO: convert film to camera's coordinate system. Currently the
  --film is on the global xz plane.
  where direction = (dir cam) `vadd` (Vec
                                      (i / fromIntegral (width img) - 0.5)
                                      0
                                      (-1 * (j / fromIntegral (height img) - 0.5)))

jitter :: RandomGen g => g -> Int -> Double
jitter rand n = (r - 0.5) + realToFrac n
  where r = fst $ random rand

multipleRays :: RandomGen g => Int -> World -> ImageProperties -> Int -> Int -> g -> Pixel RGB Double
multipleRays count world img i j rand
  = expAvg $ take count $ fmap (singleRay . split3) $ mkRands rand
  where
    singleRay = (\(r1, r2, r3) -> rayTrace world (film img (camera world) (jitter r1 i) (jitter r2 j)) r3 (maxBounces img))

    --TODO: properly manage linear/logarithmic lighting
    expAvg :: [Pixel RGB Double] -> Pixel RGB Double
    expAvg = brighten . (**(1/lightPower)) . foldr (+) 0 . fmap (/ (realToFrac count)) . fmap (**lightPower)

    lightPower :: Pixel RGB Double
    lightPower = 3
    brighten = (**0.4)

render :: RandomGen g => ImageProperties -> World -> g -> Image VU RGB Double
render img world rand = makeImage (height img, width img)
  (\(j,i) -> multipleRays (raysPerPixel img) world img i j (rands ! (i,j)))
  where
    rands = splitMany rand (width img) (height img)

split3 :: RandomGen g => g -> (g, g, g)
split3 (split -> (r1, r2)) = (r2, r3, r4) where
  (r3, r4) = split r1

splitMany :: RandomGen g => g -> Int -> Int -> Array (Int, Int) g
splitMany rand x y = listArray ((0,0), (x,y)) (mkRands rand)

mkRands :: RandomGen g => g -> [g]
mkRands = unfoldr (pure . split)

black :: Colour
black = 0
