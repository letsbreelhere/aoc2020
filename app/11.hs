module Main where

import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Control.Monad
import Linear.V2
import Linear.Vector
import Dir
import Util

data Tile = Floor | Empty | Occupied
  deriving (Show, Eq)

type Grid = Map Point Tile

parseGrid :: [String] -> Grid
parseGrid = M.fromList . map (\(c, p) -> (p, parseTile c)) . zipWithCoordinates
  where parseTile '.' = Floor
        parseTile '#' = Occupied
        parseTile 'L' = Empty

cardinals :: [Point]
cardinals = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  guard (abs dx + abs dy > 0)
  pure (V2 dx dy)

neighbors :: Point -> [Point]
neighbors p = (p +) <$> cardinals

neighborCount :: Point -> Grid -> Int
neighborCount p g =
  let ns = neighbors p
   in length . filter (\n -> M.lookup n g == Just Occupied) $ ns

step :: Grid -> Grid
step g = M.fromList . map (\(p, t) -> (p, stepTile p t g)) . M.toList $ g
  where stepTile _ Floor _ = Floor
        stepTile p Empty g = if neighborCount p g == 0 then Occupied else Empty
        stepTile p Occupied g = if neighborCount p g >= 4 then Empty else Occupied

ray :: Point -> [Point]
ray p = map (p ^*) [1..]

hasNeighbor :: Point -> Point -> Grid -> Bool
hasNeighbor p c g = go (ray c)
  where go (c:cs) = case M.lookup (p + c) g of
                      Just Occupied -> True
                      Just Empty -> False
                      Nothing -> False
                      _ -> go cs

complicatedNeighborCount :: Point -> Grid -> Int
complicatedNeighborCount p g = length . filter (\c -> hasNeighbor p c g) $ cardinals

complicatedStep :: Grid -> Grid
complicatedStep g = M.fromList . map (\(p, t) -> (p, stepTile p t g)) . M.toList $ g
  where stepTile _ Floor _ = Floor
        stepTile p Empty g = if complicatedNeighborCount p g == 0 then Occupied else Empty
        stepTile p Occupied g = if complicatedNeighborCount p g >= 5 then Empty else Occupied

occupiedCount :: Grid -> Int
occupiedCount = length . filter (\(_, t) -> t == Occupied) . M.toList

pretty :: Grid -> String
pretty g = unlines $ do
  y <- [0..9]
  pure $ do
    x <- [0..9]
    let Just t = M.lookup (V2 x y) g
    pure $ case t of
      Floor -> '.'
      Occupied -> '#'
      Empty -> 'L'

main :: IO ()
main = do
  input <- parseGrid . lines <$> readFile "inputs/11.txt"
  print . occupiedCount . steadyState step $ input
  print . occupiedCount . steadyState complicatedStep $ input
