module Main where

import Data.List
import Control.Monad

count x = length . filter (== x)

diffs xs = zipWith (-) (tail xs) xs

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = do
  sl <- sublists xs
  [x:sl, sl]

arrangements :: [Int] -> [[Int]]
arrangements xs = do
  xs' <- sublists xs
  guard (not $ null xs')
  guard (head xs' == head xs)
  guard (last xs' == last xs)
  guard (maximum (0 : diffs xs') <= 3)
  pure xs'

contiguousSublists' :: [Int] -> [Int] -> [[Int]]
contiguousSublists' [] [] = []
contiguousSublists' [] (y:ys) = contiguousSublists' [y] ys
contiguousSublists' xs [] = [xs]
contiguousSublists' xs (y:ys)
  | y - last xs > 1 = xs : contiguousSublists' [y] ys
  | otherwise = contiguousSublists' (xs ++ [y]) ys

contiguousSublists :: [Int] -> [[Int]]
contiguousSublists = contiguousSublists' []

main :: IO ()
main = do
  is <- sort . map (read :: String -> Int) . lines <$> readFile "inputs/10.txt"
  let input = 0 : is ++ [last is + 3]
      ds = diffs input
  print $ count 1 ds * count 3 ds
  print . product . map (length . arrangements) $ contiguousSublists input
