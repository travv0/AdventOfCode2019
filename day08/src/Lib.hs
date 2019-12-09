module Lib where

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
