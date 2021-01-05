{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (replicateM)
import Data.Functor (($>))
import Control.Applicative ((<|>))

data Bag = Bag String [(Int, String)]
  deriving (Show)

word = many1 letter

parseBag :: Parser Bag
parseBag = do
  color <- unwords <$> replicateM 2 (word <* skipSpace)
  string "bags contain "
  recur <- (string "no other bags." $> []) <|> subBags
  pure (Bag color recur)
  where
    subBags = do
      bs <- sepBy1 rule (string ", ")
      string "."
      pure bs
    rule = do
      cnt <- decimal
      skipSpace
      color <- unwords <$> replicateM 2 (word <* skipSpace)
      if cnt == 1
        then string "bag"
        else string "bags"
      pure (cnt, color)

type Graph = Map String [(Int, String)]

eventuallyGold :: Graph -> String -> Bool
eventuallyGold _ "shiny gold" = True
eventuallyGold g s =
  case Map.lookup s g of
    Nothing -> False
    Just bs -> any (eventuallyGold g . snd) bs

main :: IO ()
main = do
  input <- map ((\(Right x) -> x) . parseOnly parseBag . T.pack) . lines <$> readFile "inputs/7.txt"
  let graph = Map.fromList $ map (\(Bag c bs) -> (c, bs)) input

  -- Part 1
  print . length . filter id . map (eventuallyGold graph) . filter (/= "shiny gold") $ Map.keys graph

  -- Part 2
  print $ subBagCount graph "shiny gold" - 1

subBagCount :: Graph -> String -> Int
subBagCount g s =
  case Map.lookup s g of
    Nothing -> 0
    Just bs -> (1 +) . sum . map (\(cnt, clr) -> cnt * subBagCount g clr) $ bs
