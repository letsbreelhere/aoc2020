module Main where

import qualified Data.Set as S
import Control.Applicative
import Data.List

isSumOf :: Int -> [Int] -> Bool
isSumOf x ys = any (\y -> S.member (x - y) (S.fromList ys)) ys

summingToFromStart :: [Int] -> Int -> Maybe [Int]
summingToFromStart _ 0 = Just []
summingToFromStart [] _ = Nothing
summingToFromStart (x:xs) y
  | x <= y = fmap (x :) (summingToFromStart xs (y - x))
  | otherwise = Nothing

summingTo :: [Int] -> Int -> Maybe [Int]
summingTo _ 0 = Just []
summingTo [] _ = Nothing
summingTo xs y = summingToFromStart xs y <|> summingTo (tail xs) y

slice :: Int -> Int -> [a] -> [a]
slice start finish = take (finish - 1) . drop start

main :: IO ()
main = do
  input <- map (read :: String -> Int) . lines <$> readFile "inputs/9.txt"
  let ixs = [25..length input - 1]
  let cond ix = let cur = input !! ix
                    preceding = slice (ix - 25) (ix - 1) input
                in not (cur `isSumOf` preceding)

  let Just invalid = (input !!) <$> find cond ixs
  print invalid

  let Just seq = summingTo input invalid
  print $ minimum seq + maximum seq
