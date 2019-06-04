module Image where

import Graphics.Image as I
import System.Random

import Ray

renderScene :: ImageProperties -> World -> IO (Image VU RGB Double)
renderScene props scene = tosRGB . render props scene <$> newStdGen

writeSceneImage :: ImageProperties -> World -> FilePath -> IO ()
writeSceneImage props scene filename = writeImage filename =<< renderScene props scene

tosRGB :: Image VU RGB Double -> Image VU RGB Double
tosRGB = I.map (fmap ((255*) . f)) where
  f :: Double -> Double
  f u | u <= 0.0031308 = 12.92 * u
      | otherwise      = 1.055 * u**(1/2.4) - 0.055
