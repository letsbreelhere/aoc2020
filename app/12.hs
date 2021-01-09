module Main where

import Data.Attoparsec.Text
import qualified Data.Text.IO as T
import Dir
import Data.List
import Control.Applicative
import Linear.Vector
import Linear.V2

data Action = Turning [Turn]
            | Move Dir Int
            | Forward Int
            deriving (Show)

oneOf = fmap (:[]) . choice . map char

parseAction :: Parser Action
parseAction = parseTurn <|> parseMove <|> parseForward
  where
    parseTurn = do
      turn <- read <$> oneOf "LR"
      deg <- decimal
      let count = deg `div` 90
      pure $ Turning (replicate count turn)
    parseMove = do
      Move <$> fmap read (oneOf "NEWS") <*> decimal
    parseForward = do
      char 'F'
      Forward <$> decimal

parseFile :: Parser [Action]
parseFile = sepBy parseAction skipSpace

applyAction :: (Point, Dir) -> Action -> (Point, Dir)
applyAction (p, d) (Turning ts) = (p, foldr turnDir d ts)
applyAction (p, d) (Move dir n) = (p + (vectorRep dir ^* n), d)
applyAction (p, d) (Forward n) = (p + (vectorRep d ^* n), d)

main :: IO ()
main = do
  Right input <- parseOnly parseFile <$> T.readFile "inputs/12.txt"
  let V2 x y = fst $ foldl' applyAction (V2 0 0, Dir.E) input
  print $ abs x + abs y
