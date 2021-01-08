module Main where

import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad
import Linear.V2
import Dir

data Tile = Floor | Empty | Occupied
  deriving (Show, Eq)

type Grid = Map Point Tile

parseGrid :: [String] -> Grid
parseGrid = M.fromList . map (\(c, p) -> (p, parseTile c)) . zipWithCoordinates
  where parseTile '.' = Floor
        parseTile '#' = Occupied
        parseTile 'L' = Empty

neighbors :: Point -> [Point]
neighbors p = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  guard (abs dx + abs dy > 0)
  pure $ p + V2 dx dy

neighborCount :: Point -> Grid -> Int
neighborCount p g =
  let ns = neighbors p
   in length . filter (\n -> M.lookup n g == Just Occupied) $ ns

step :: Grid -> Grid
step g = M.fromList . map (\(p, t) -> (p, stepTile p t g)) . M.toList $ g
  where stepTile _ Floor _ = Floor
        stepTile p Empty g = if neighborCount p g == 0 then Occupied else Empty
        stepTile p Occupied g = if neighborCount p g >= 4 then Empty else Occupied

steadyState :: Grid -> Grid
steadyState g =
  let g' = step g
   in if g == g'
         then g
         else steadyState (step g')

occupiedCount :: Grid -> Int
occupiedCount = length . filter (\(_, t) -> t == Occupied) . M.toList

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/11.txt"
  print . occupiedCount . steadyState . parseGrid $ input
