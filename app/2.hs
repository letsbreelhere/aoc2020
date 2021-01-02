{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Attoparsec.Text
import qualified Data.Text as T

data Entry = Entry { lo :: Int, hi :: Int, c :: Char, s :: String }
  deriving (Show)

isValidPt1 :: Entry -> Bool
isValidPt1 Entry { lo, hi, c, s } =
  let count = length . filter (== c) $ s
   in count >= lo && count <= hi

isValidPt2 :: Entry -> Bool
isValidPt2 Entry { lo, hi, c, s } =
  case (s !! (lo-1) == c, s !! (hi-1) == c) of
    (True, False) -> True
    (False, True) -> True
    _ -> False

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
  print . length . filter id . map isValidPt1 $ parsed
  -- Part 2
  print . length . filter id . map isValidPt2 $ parsed
