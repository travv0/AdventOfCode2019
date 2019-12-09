module Lib where

import           Control.Monad
import           Data.List.Split

type Layer = [[Int]]

type Image = [Layer]

makeImage :: Int -> Int -> String -> Image
makeImage width height input =
  let intInput = map (read . pure) input
      layers   = chunksOf (width * height) intInput
  in  map (chunksOf width) layers

countDigits :: Int -> Layer -> Int
countDigits digit =
  foldr (\row digits -> digits + length (filter (== digit) row)) 0

layerWithLeastOfDigit :: Int -> Image -> Layer
layerWithLeastOfDigit digit image = fst $ foldr
  (\layer minLayer@(_, i) ->
    let digitCount = countDigits digit layer
    in  if digitCount < i then (layer, digitCount) else minLayer
  )
  (firstLayer, countDigits digit firstLayer)
  (tail image)
  where firstLayer = head image

groupPixels :: Image -> [[[Int]]]
groupPixels image = foldr
  combineLayers
  (replicate (length $ head image) (replicate (length $ head $ head image) []))
  image

combineLayers :: [[Int]] -> [[[Int]]] -> [[[Int]]]
combineLayers = zipWith $ zipWith (:)

squashLayers :: [[[Int]]] -> [[Int]]
squashLayers = map $ map (head . filter (< 2))

printFinalImage :: [[Int]] -> IO ()
printFinalImage = mapM_ $ \row -> do
  forM_ row $ \pixel -> putStr $ case pixel of
    1 -> "."
    _ -> " "
  putStrLn ""
