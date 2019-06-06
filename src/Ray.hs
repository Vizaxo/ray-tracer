module Ray where

import Data.Array
import Data.Glome.Vec as V
import Data.List hiding (intersect)
import Data.Maybe
import Data.Ord
import Graphics.Image hiding (Array, map, traverse)

import Materials
import Rand
import Utils

tau :: Floating a => a
tau = 2 * pi

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
    , let t = -1 * ((n `vdot` ro) + d / denom)
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
rayTrace :: World -> Int -> RefractiveIndex -> Ray -> Rand (Pixel RGB Double)
rayTrace w 0 ri ray = pure black
rayTrace w maxRays ri ray = processHits $ concat $ getHits <$> objects w
  where
    getHits :: Object -> [(Hit, Material)]
    getHits o = ((,material o) <$> intersect ray (shape o))

    processHits :: [(Hit, Material)] -> Rand Colour
    processHits = mkColour . listToMaybe
      -- TODO: maximum, not sort
      . sortBy (comparing $ (\v -> vlen (origin ray `vsub` v)) . hitPos . fst)

    mkColour :: Maybe (Hit, Material) -> Rand Colour
    mkColour Nothing = pure (diffuseColour (sky w))
    mkColour (Just (Hit hitPos hitNorm entering, mat)) = do
      p <- getRand
      nextRayContrib <- if (diffuseColour mat /= 0)
        then
          if p <= transparency mat
            then refractedContrib
            else reflectedContrib
        else pure (PixelRGB 0 0 0)
      pure $ emissionColour mat + nextRayContrib
     where
       --TODO: Russian Roulette
       refractedContrib :: Rand Colour
       refractedContrib = (* 1.15) <$> recurse riRefract (Ray hitPos refractedRay)

       reflectedContrib :: Rand Colour
       reflectedContrib = do
         ray <- reflectedRay
         nextRayContrib <- recurse ri (Ray hitPos ray)
         pure (nextRayContrib
               * diffuseColour mat
              --TODO: fix reflection from pure specular not in right direction
               * realToFrac (ray `vdot` hitNorm))

       recurse = rayTrace w (maxRays - 1)

       -- TODO: handle exiting into different material. Stack of RIs?
       riRefract = if entering then refractiveIndex mat else 1

       perfectReflect = reflect (dir ray) hitNorm

       refractedRay = snell (dir ray) hitNorm ri (refractiveIndex mat)

       reflectedRay = do
         randomRay <- vnorm <$> sampleHemisphere hitNorm
         pure (vnorm (vlerp (specular mat) randomRay perfectReflect))

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
sampleHemisphere :: Vec -> Rand Vec
sampleHemisphere v = do
  u <- sampleSphere
  pure (if v `vdot` u >= 0 then u else (vinvert u))

-- Uniform sampling of points on a sphere.
sampleSphere :: Rand Vec
sampleSphere = do
  u1 <- getRand
  u2 <- getRand
  let r = sqrt (1 - u1*u1)
      phi = tau * u2
  pure (Vec (cos phi * r) (sin phi * r) (2 * u1))

-- Linear interpolation between two fractionals.
lerp :: Fractional n => Double -> n -> n -> n
lerp l a b = (realToFrac $ 1 - l) * a + realToFrac l * b

-- Linear interpolation between two vectors.
-- TODO: this is just pontwise-lerping; should a uniform vector lerp be used?
vlerp :: Double -> Vec -> Vec -> Vec
vlerp l = vmap2 (lerp l)

--Calculate the ray to cast for the given pixel.
film :: ImageProperties -> Ray -> Double -> Double -> Ray
film img cam i j = Ray (origin cam `vadd` direction) (vnorm direction)
  --TODO: convert film to camera's coordinate system. Currently the
  --film is on the global xz plane.
  where direction = dir cam `vadd` (Vec x 0 z)
        x = (i / fromIntegral (width img) - 0.5)
        z = (-1 * (j / fromIntegral (height img) - 0.5))

-- Jitter a ray by +/-0.5px for anti-aliasing.
jitter :: Int -> Rand Double
jitter n = do
  r <- getRand
  pure ((r - 0.5) + realToFrac n)

-- Fire a given number of rays through the world and calculate the final pixel
-- value.
multipleRays ::
  Int -> World -> ImageProperties -> Int -> Int -> Rand (Pixel RGB Double)
multipleRays count world img i j
  = avg <$> doTimes count singleRay
  where
    singleRay = do
      ji <- jitter i
      jj <- jitter j
      rayTrace world (maxBounces img) 1 (film img (camera world) ji jj)

    avg :: [Pixel RGB Double] -> Pixel RGB Double
    avg =  foldr (+) 0 . fmap (/ (realToFrac count))

-- Render a scene into an image.
render :: ImageProperties -> World -> Rand (Image VU RGB Double)
render img world = do
  let bounds = ((0,0), (width img - 1, height img - 1))
      indices = range bounds
  results <- traverse calcPixel indices
  let arr = listArray bounds results
  pure (makeImage (height img, width img) (\(j,i) -> arr ! (i, j)))
  where
    calcPixel (i, j)
      | flatColours img = pure (traceFlat world img i j)
      | otherwise = multipleRays (raysPerPixel img) world img i j
