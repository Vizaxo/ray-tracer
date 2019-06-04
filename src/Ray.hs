module Ray where

import Data.Array
import Data.Glome.Vec as V
import Graphics.Image hiding (Array, map)
import Data.List hiding (intersect)
import Data.Ord
import Data.Maybe
import System.Random

tau :: Floating a => a
tau = 2 * pi

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

data Shape
  = Sphere {centre :: Vec, radius :: Flt}
  | Plane {normal :: Vec, d :: Flt}
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
  , flatColours :: Bool
  }
  deriving Show

data Hit = Hit
  { hitPos :: Vec
  , surfaceNormal :: Vec
  , isEntering :: Bool
  }
  deriving Show

black :: Colour
black = 0

-- Get all of the intersections between a ray and a shape
-- TODO: return t, so the closest one can be calculated more efficiently
intersect :: Ray -> Shape -> [Hit]
intersect (Ray rayOrigin rd) (Sphere centre r)
  = [ Hit hitPos surfaceNormal entered
    | let ro = rayOrigin `vsub` centre
          -- a = rd `vdot` rd = 1 (rd is always a unit vector)
          b = 2 * (rd `vdot` ro)
          c = (ro `vdot` ro) - r^2
          root = (sqrt (b^2 - 4*c))
    , plusMinus <- [(-1), 1]
    , let t = (-b + (plusMinus * root)) / 2
    , t > 0.00001
    , let hitPos = rayOrigin `vadd` (rd `vscale` t)
          surfaceNormal = vnorm (hitPos `vsub` centre)
          entered = rd `vdot` surfaceNormal <= 0
    ]
intersect (Ray ro rd) (Plane n d)
  = [ Hit hitPos surfaceNormal True --TODO: work out plane entry/exit
    | let denom = vdot n rd
    , denom /= 0
    , let t = ((n `vdot` ro) + d / denom)
    , t > 0.00001
    , let hitPos = ro `vadd` (rd `vscale` t)
          surfaceNormal = if rd `vdot` n <= 0 then n else vinvert n
    ]

-- Quickly trace a ray returning a flat colour
traceFlat :: World -> ImageProperties -> Int -> Int -> Pixel RGB Double
traceFlat w img i j = processHits $ concat $ getHits <$> objects w
  where
    ray = film img (camera w) (realToFrac i) (realToFrac j)

    getHits :: Object -> [(Hit, Material)]
    getHits o = ((,material o) <$> intersect ray (shape o))

    processHits :: [(Hit, Material)] -> Colour
    processHits = mkColour . listToMaybe
      -- TODO: maximum, not sort
      . sortBy (comparing $ (\v -> vlen (origin ray `vsub` v)) . hitPos . fst)

    mkColour Nothing = diffuseColour (sky w)
    mkColour (Just (Hit hitPos hitNorm entering, mat))
      = emissionColour mat + (diffuseColour mat * 0.0003)

-- Trace a ray through the world, calculating its final pixel contribution,
-- starting from a material with refractive index ri.  It will calculate no more
-- than maxRays rays.
rayTrace :: RandomGen g
  => World -> g -> Int -> RefractiveIndex -> Ray -> Pixel RGB Double
rayTrace w rand 0 ri ray = black
rayTrace w rand maxRays ri ray = processHits $ concat $ getHits <$> objects w
  where
    getHits :: Object -> [(Hit, Material)]
    getHits o = ((,material o) <$> intersect ray (shape o))

    processHits :: [(Hit, Material)] -> Colour
    processHits = mkColour . listToMaybe
      -- TODO: maximum, not sort
      . sortBy (comparing $ (\v -> vlen (origin ray `vsub` v)) . hitPos . fst)

    mkColour Nothing = diffuseColour (sky w)
    mkColour (Just (Hit hitPos hitNorm entering, mat))
      = emissionColour mat
      + if (diffuseColour mat /= 0)
        then (if p <= transparency mat
              then refractedContrib
              else reflectedContrib)
        else 0
     where
       --TODO: Russian Roulette
       refractedContrib = recurse riRefract (Ray hitPos refractedRay) * 1.15
       reflectedContrib = recurse ri (Ray hitPos reflectedRay)
         * diffuseColour mat
         --TODO: fix reflection from pure specular not in right direction
         * realToFrac (reflectedRay `vdot` hitNorm)

       recurse = rayTrace w rand2 (maxRays - 1)

       -- TODO: handle exiting into different material. Stack of RIs?
       riRefract = if entering then refractiveIndex mat else 1

       perfectReflect = reflect (dir ray) hitNorm

       refractedRay = snell (dir ray) hitNorm ri (refractiveIndex mat)

       (p, rand2) = random rand1

       (reflectedRay, rand1) = let
         (r1, r2) = split rand
         randomRay = vnorm (sampleHemisphere r1 hitNorm)
         in (vnorm (vlerp (specular mat) randomRay perfectReflect), r2)

-- Snell's Law gives the refracted vector when a ray travels through a boundary
-- TODO: check for total internal reflection
snell :: Vec -> Vec -> RefractiveIndex -> RefractiveIndex -> Vec
snell incident surfaceNorm n1 n2
  = (normal `vcross` (vinvert normal `vcross` incident) `vscale` r)
  `vsub` (normal `vscale` sqrt rootTerm)
  where
    normal = if incident `vdot` surfaceNorm <= 0
      then surfaceNorm else vinvert surfaceNorm
    rootTerm = 1 - (r^2) * (vlensqr (normal `vcross` incident))
    r = n1 / n2

-- Uniform sampling of points of a hemisphere with its pole in the
-- direction of v.
sampleHemisphere :: RandomGen g => g -> Vec -> Vec
sampleHemisphere rand v = if v `vdot` u >= 0 then u else (vinvert u)
  where u = sampleSphere rand

-- Uniform sampling of points on a sphere.
sampleSphere :: RandomGen g => g -> Vec
sampleSphere rand = Vec (cos phi * r) (sin phi * r) (2 * u1)
  where (u1, rand') = random rand
        r = sqrt (1 - u1*u1)
        (u2, _) = random rand'
        phi = tau * u2

-- Linear interpolation between two fractionals.
lerp :: Fractional n => Double -> n -> n -> n
lerp l a b = (realToFrac $ 1 - l) * a + realToFrac l * b

-- Linear interpolation between two vectors.
-- TODO: this is just pontwise-lerping; should a uniform vector lerp be used?
vlerp :: Double -> Vec -> Vec -> Vec
vlerp l = vmap2 (lerp l)

--Calculate the ray to cast for the given pixel.
film :: ImageProperties -> Ray -> Double -> Double -> Ray
film img cam i j = Ray (origin cam) (vnorm direction)
  --TODO: convert film to camera's coordinate system. Currently the
  --film is on the global xz plane.
  where direction = dir cam `vadd` (Vec x 0 z)
        x = (i / fromIntegral (width img) - 0.5)
        z = (-1 * (j / fromIntegral (height img) - 0.5))

-- Jitter a ray by +/-0.5px for anti-aliasing.
jitter :: RandomGen g => g -> Int -> Double
jitter rand n = (r - 0.5) + realToFrac n
  where r = fst $ random rand

-- Fire a given number of rays through the world and calculate the final pixel
-- value.
multipleRays :: RandomGen g
  => Int -> World -> ImageProperties -> Int -> Int -> g -> Pixel RGB Double
multipleRays count world img i j rand
  = avg $ take count $ singleRay . split3 <$> mkRands rand
  where
    singleRay (r1, r2, r3) = rayTrace world r3 (maxBounces img) 1
      (film img (camera world) (jitter r1 i) (jitter r2 j))

    avg :: [Pixel RGB Double] -> Pixel RGB Double
    avg =  foldr (+) 0 . fmap (/ (realToFrac count))

-- Render a scene into an image.
render :: RandomGen g => ImageProperties -> World -> g -> Image VU RGB Double
render img world rand = makeImage (height img, width img)
  (\(j,i) -> calcPixel i j)
  where
    getRand i j = splitMany rand (width img) (height img) ! (i,j)
    calcPixel i j
      | flatColours img = traceFlat world img i j
      | otherwise = multipleRays (raysPerPixel img) world img i j (getRand i j)

-- Split a random number generator into 3 new generators.
split3 :: RandomGen g => g -> (g, g, g)
split3 (split -> (r1, r2)) = (r2, r3, r4)
  where (r3, r4) = split r1

-- Split a random number generator into an array of generators.
splitMany :: RandomGen g => g -> Int -> Int -> Array (Int, Int) g
splitMany rand x y = listArray ((0,0), (x,y)) (mkRands rand)

-- Split a random number generator into an infinite list of generators.
mkRands :: RandomGen g => g -> [g]
mkRands = unfoldr (pure . split)
