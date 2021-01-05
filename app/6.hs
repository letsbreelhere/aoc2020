module Main where

import Data.List.Split (splitOn)
import qualified Data.List as L

agreeCount :: [String] -> Int
agreeCount = length . foldr L.intersect ['a'..'z']

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> readFile "inputs/6.txt"

  -- Part 1
  print . sum . map (length . L.nub . filter (/= '\n')) $ input

  -- Part 2
  print . sum . map (agreeCount . filter (not . null) . splitOn "\n") $ input
