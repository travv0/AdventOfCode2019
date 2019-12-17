module Main where

import           Data.Char
import           Lib

main :: IO ()
main = do
  input <- filter (not . isSpace) <$> readFile "input.txt"
  let resultDigits = fft 100 [0, 1, 0, -1] $ digits input
  putStrLn $ map intToDigit $ take 8 resultDigits
