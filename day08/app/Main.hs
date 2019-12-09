module Main where

import           Lib
import           Data.Char

main :: IO ()
main = do
  input <- filter (not . isSpace) <$> readFile "input.txt"
  let image = makeImage 25 6 input
  part1 image
  part2 image

part1 :: Image -> IO ()
part1 image = do
  let layer = layerWithLeastOfDigit 0 image
  putStr "Part 1: "
  print $ countDigits 1 layer * countDigits 2 layer

part2 :: Image -> IO ()
part2 image = do
  putStrLn "Part 2:"
  printFinalImage $ squashLayers $ groupPixels image
