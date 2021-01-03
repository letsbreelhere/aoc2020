module Main where

import Data.Bits
import Data.List

bitsOf :: String -> [Bool]
bitsOf = map bit
  where bit 'B' = True
        bit 'R' = True
        bit _ = False

rowId :: String -> Int
rowId = foldl' (\acc b -> acc `shiftL` 1 + (if b then 1 else 0)) 0 . bitsOf

main :: IO ()
main = do
  input <- map rowId . lines <$> readFile "inputs/5.txt"

  let minId = minimum input
      maxId = maximum input

  -- Part 1
  print maxId

  -- Part 2
  print $ head ([minId..maxId] \\ input)
