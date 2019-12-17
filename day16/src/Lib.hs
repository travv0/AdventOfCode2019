module Lib where

import           Data.Char

createPattern :: Int -> [Int] -> [Int]
createPattern pos = tail . cycle . concatMap (replicate pos)

digits :: String -> [Int]
digits = map digitToInt

applyPattern :: [Int] -> [Int] -> [Int]
applyPattern basePattern dgts = map calcNewDigit [1 .. length dgts]
 where
  calcNewDigit i =
    abs $ (`rem` 10) $ sum $ zipWith (*) dgts $ createPattern i basePattern

fft :: Int -> [Int] -> [Int] -> [Int]
fft 0 _ dgts = dgts
fft numOfPhases basePattern dgts =
  fft (pred numOfPhases) basePattern $ applyPattern basePattern dgts
