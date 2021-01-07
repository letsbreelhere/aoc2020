module Main where

import qualified Data.Set as S
import Data.Set (Set)
import Data.List

isSumOf :: Int -> [Int] -> Bool
isSumOf x ys = any (\y -> S.member (x - y) (S.fromList ys)) ys

slice :: Int -> Int -> [a] -> [a]
slice start finish = take (finish - 1) . drop start

main :: IO ()
main = do
  input <- map (read :: String -> Int) . lines <$> readFile "inputs/9.txt"
  let ixs = [25..length input - 1]
  let cond ix = let cur = input !! ix
                    preceding = slice (ix - 25) (ix - 1) input
                in not (cur `isSumOf` preceding)

  print . fmap (input !!) $ find cond ixs
