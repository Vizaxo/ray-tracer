module Ray where

import Data.Glome.Vec as V
import Graphics.Image hiding (map)
import Debug.Trace
import Data.Maybe
import Data.List hiding (intersect)
import Data.Ord

data Object = Sphere {centre :: Vec, radius :: Float}
            | Plane {normal :: Vec, point :: Vec, colour :: Colour}
  deriving Show

type Colour = Pixel RGB Double

type World = [Object]

pixW = 480
pixH = 480

intersect :: Ray -> Object -> [Vec]
intersect ray (Sphere _ _) = undefined
intersect (Ray ro rd) (Plane n d c) = [ro `vadd` (rd `vscale` t)
                                    | let denom = vdot n rd
                                    , denom /= 0
                                    , let t = (vdot n (ro `vsub` d)) / denom
                                    , t > 0
                                    ]

rayTrace :: World -> Int -> Int -> Pixel RGB Double
rayTrace w i j = toColor $ concat $ (removeEmpties . (\o -> (intersect (film cam i j) o, colour o))) <$> w
  where toColor :: [(Vec, Colour)] -> Colour
        toColor [] = PixelRGB 0.2 0.4 0.7 -- sky
        toColor vs = (\(dist, c) -> PixelRGB (dist / 100) 0.5 0.5) $ head $ sortBy (comparing fst) $ map (\(v, c) -> (vlen (camPos `vsub` v), c)) vs --List of all intersections. Pick the closest one.
        --closer :: (Vec, Colour) -> (Vec, Colour) -> Ordering
        --closer (i1, c1) (i2, c2) = comparing fst
        removeEmpties ([], c) = []
        removeEmpties (xs, c) = (,c) <$> xs

--Calculate the ray to project onto the film
film :: Ray -> Int -> Int -> Ray
film (Ray pos dir) i j = Ray pos dir'
  where dir' = (vnorm dir) `vadd` (Vec (fromIntegral i / fromIntegral pixW - (0.5)) 0 (fromIntegral j / fromIntegral pixH - (0.5)))

cam :: Ray
cam = Ray camPos camDir --(vnorm (vzero `vsub` pos))

camPos = Vec 0 10 1
camDir = Vec 1 (-0.1) (-0.1)

render :: World -> Image VU RGB Double
render world = makeImage (pixH,pixW) (\(i,j) -> rayTrace world j i)

testImage :: Image VU RGB Double
testImage = render
  [ Plane (Vec 0.002 0.0 0.1) (Vec 0 (-00) 0) (PixelRGB 0.8 0.2 0.0)
  , Plane (Vec 0.3 (0.5) 1) (Vec 0 (-10) 0) (PixelRGB 0.1 0.8 0.0)
  , Plane (Vec 0.03 0.60 1) (Vec 0 (-00) 0) (PixelRGB 0.4 0.4 0.4)
  ]

main :: IO ()
main = do writeImage "test.png" testImage
          return ()
