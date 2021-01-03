module Main where

import Data.Bits
import Data.List

rowId :: String -> Int
rowId = foldl' (\acc b -> acc `shiftL` 1 + b) 0 . map bit
  where bit 'B' = 1
        bit 'R' = 1
        bit _ = 0

main :: IO ()
main = do
  input <- map rowId . lines <$> readFile "inputs/5.txt"

  let minId = minimum input
      maxId = maximum input

  -- Part 1
  print maxId

  -- Part 2
  print $ head ([minId..maxId] \\ input)
