module Main where

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Process

import Image
import Ray
import Scene

width' = 200
height' = 200
raysPerPixel' = 20
maxBounces' = 10

benchmarkImg :: ImageProperties
benchmarkImg = ImageProperties
  { width = width'
  , height = height'
  , raysPerPixel = raysPerPixel'
  , maxBounces = maxBounces'
  , flatColours = False
  }

main :: IO ()
main = do
  putStrLn $ "Benchmark settings: "
  putStrLn $ "\twidth: " <> show width'
  putStrLn $ "\theight: " <> show height'
  putStrLn $ "\traysPerPixel: " <> show raysPerPixel'
  putStrLn $ "\tmaxBounces: " <> show maxBounces'
  startTime <- getPOSIXTime
  system "mkdir -p output"
  writeSceneImage benchmarkImg cornellBox "output/benchmark.png"
  endTime <- getPOSIXTime
  let tdiff = endTime - startTime
  putStrLn $ "Image written to output/benchmark.png"
  putStrLn $ "Time taken: " <> show tdiff
  putStrLn $ "Time per sample: "
    <> show (tdiff / realToFrac (width' * height' * raysPerPixel'))
