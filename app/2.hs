{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Attoparsec.Text
import qualified Data.Text as T

data Entry = Entry { lo :: Int, hi :: Int, c :: Char, s :: String }
  deriving (Show)

isValid :: Entry -> Bool
isValid Entry { lo, hi, c, s } =
  let count = length . filter (== c) $ s
   in count >= lo && count <= hi

entry :: Parser Entry
entry = do
  lo <- decimal
  char '-'
  hi <- decimal
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  pure Entry { lo, hi, c, s }

main :: IO ()
main = do
  let parseLine = parseOnly entry
  inputs <- map parseLine . T.lines . T.pack <$> readFile "inputs/2.txt"
  let parsed = map (\(Right e) -> e) inputs

  -- Part 1
  print . length . filter id . map isValid $ parsed
