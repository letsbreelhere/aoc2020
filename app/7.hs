{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (replicateM)
import Data.Functor (($>))
import Control.Applicative ((<|>))

type Bag = (String, [(Int, String)])

word = many1 letter
color = unwords <$> replicateM 2 (word <* skipSpace)

parseBag :: Parser Bag
parseBag = do
  clr <- color
  string "bags contain "
  recur <- (string "no other bags." $> []) <|> subBags
  pure (clr, recur)
  where
    subBags = do
      bs <- sepBy1 rule (string ", ")
      string "."
      pure bs
    rule = do
      cnt <- decimal
      skipSpace
      clr <- color
      if cnt == 1
        then string "bag"
        else string "bags"
      pure (cnt, clr)

type Graph = Map String [(Int, String)]

eventuallyGold :: Graph -> String -> Bool
eventuallyGold _ "shiny gold" = True
eventuallyGold g s =
  case Map.lookup s g of
    Nothing -> False
    Just bs -> any (eventuallyGold g . snd) bs

main :: IO ()
main = do
  input <- map ((\(Right x) -> x) . parseOnly parseBag) . T.lines <$> T.readFile "inputs/7.txt"
  let graph = Map.fromList input

  -- Part 1
  print . subtract 1 . length . filter (eventuallyGold graph) $ Map.keys graph

  -- Part 2
  print $ subBagCount graph "shiny gold" - 1

subBagCount :: Graph -> String -> Int
subBagCount g s =
  case Map.lookup s g of
    Nothing -> 0
    Just bs -> (1 +) . sum . map (\(cnt, clr) -> cnt * subBagCount g clr) $ bs
