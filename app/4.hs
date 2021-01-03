module Main where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Control.Monad (replicateM)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

type Entry = Map Text Text

pair :: Parser (Text, Text)
pair = do
  key <- T.pack <$> replicateM 3 letter
  char ':'
  val <- T.pack <$> many1 (satisfy (not . isSpace))
  pure (key, val)

entry :: Parser Entry
entry = do
  ps <- pair `sepBy` (char ' ' <|> char '\n')
  pure (M.fromList ps)

requiredKeys =
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  ]

main :: IO ()
main = do
  input <- map T.pack . splitOn "\n\n" <$> readFile "inputs/4.txt"
  let parsed = map (parseOnly entry) input
      parsed' = map (\(Right x) -> x) parsed

  -- Part 1
  print . length . filter (\e -> all (`M.member` e) requiredKeys) $ parsed'
