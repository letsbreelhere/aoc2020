module Main where

import Data.List.Split (splitOn)
import qualified Data.List as L

main :: IO ()
main = do
  input <- map (L.nub . filter (/= '\n')) . splitOn "\n\n" <$> readFile "inputs/6.txt"
  print . sum . map length $ input
