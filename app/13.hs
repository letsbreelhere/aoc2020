module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Safe

parse :: String -> (Int, [Int])
parse s =
  let [ts, bs] = lines s
   in (read ts, mapMaybe readMay (splitOn "," bs))

solve :: Int -> [Int] -> Int
solve ts = uncurry (*) . minimumBy (comparing snd) . map (\b -> (b, b - (ts `mod` b)))

main = do
  (ts, bs) <- parse <$> readFile "inputs/13.txt"
  print $ solve ts bs
