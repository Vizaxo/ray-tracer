module Image where

import Graphics.Image as I

import Rand
import Ray
import Utils

renderScene :: ImageProperties -> World -> IO (Image VU RGB Double)
renderScene = fmap tosRGB .: runRandIO .: render

writeSceneImage :: ImageProperties -> World -> FilePath -> IO ()
writeSceneImage props scene filename = writeImage filename =<< renderScene props scene

tosRGB :: Image VU RGB Double -> Image VU RGB Double
tosRGB = I.map (fmap ((255*) . f)) where
  f :: Double -> Double
  f u | u <= 0.0031308 = 12.92 * u
      | otherwise      = 1.055 * u**(1/2.4) - 0.055
