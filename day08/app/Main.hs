module Main where

import           Lib
import           Data.Char

main :: IO ()
main = do
  input <- filter (not . isSpace) <$> readFile "input.txt"
  let image = makeImage 25 6 input
      layer = layerWithLeastOfDigit 0 image
  print $ countDigits 1 layer * countDigits 2 layer
