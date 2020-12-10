module Main where

import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad

main :: IO ()
main = do
  inputs <- fmap (map read . lines) (readFile "inputs/1.txt") :: IO [Int]

  -- Part 1
  let set = S.fromList inputs
  let Just match = L.find (\i -> S.member (2020 - i) set) inputs
  print $ match * (2020 - match)

  -- Part 2
  let match2:_ =
        do
          x <- inputs
          y <- inputs
          guard $ S.member (2020 - x - y) set
          pure (x*y*(2020 - x - y))
  print match2
