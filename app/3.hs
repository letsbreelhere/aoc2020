module Main where

import Data.Set (Set)
import qualified Data.Set as S
import Dir
import Linear.V2

type Field = Set Point

pointsInPath' :: Point -> [Point] -> Point -> Int -> [Point]
pointsInPath' p@(V2 _ y) f slope finalY
  | y >= finalY = f
  | otherwise =
    let f' = p:f
     in pointsInPath' (p + slope) f' slope finalY

pointsInPath :: Point -> Int -> [Point]
pointsInPath = pointsInPath' origin []

parseField :: [String] -> Field
parseField =
  S.fromList .
    map snd .
      filter (\(c, _) -> c == '#') .
        concat .
          zipWithCoordinates

modPoint :: Point -> Point -> Point
modPoint (V2 x y) (V2 mx my) = V2 (mod x mx) (mod y my)

treesInPath :: [String] -> Point -> Int
treesInPath input slope =
  let fieldWidth = length (head input)
      fieldHeight = length input
      bounds = V2 fieldWidth fieldHeight
      field = parseField input
      path = pointsInPath slope fieldHeight
   in length . filter (\p -> S.member (modPoint p bounds) field) $ path

main :: IO ()
main = do
  exInput <- lines <$> readFile "inputs/3.txt"
  print (treesInPath exInput (V2 3 1))
