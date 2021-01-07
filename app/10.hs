module Main where

import Data.List

count x = length . filter (== x)

main :: IO ()
main = do
  input <- sort . map (read :: String -> Int) . lines <$> readFile "inputs/10.txt"
  let diffs = zipWith (-) (tail input) input
  print $ (count 1 diffs + 1) * (count 3 diffs + 1)
