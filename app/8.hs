{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskellQuotes #-}

module Main where

import Debug.Trace
import Control.Lens ((%=), (.=), (+=), makeLenses, use)
import Control.Monad.State
import Control.Applicative
import Data.Functor
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Attoparsec.Text
import qualified Data.Text.IO as T

data Opcode = Nop
            | Acc
            | Jmp
            deriving (Show)

data ConState = ConState { _instrs :: [(Opcode, Int)], _acc :: Int, _pc :: Int, _visited :: Set Int }
makeLenses ''ConState

newtype Con a = Con { unCon :: State ConState a }
  deriving (Functor, Applicative, Monad, MonadState ConState)

runCon' :: Con a -> ConState -> (a, ConState)
runCon' = runState . unCon

runCon :: [(Opcode, Int)] -> Con a -> (a, ConState)
runCon is c = runCon' c (ConState is 0 0 Set.empty)

opcode :: Parser Opcode
opcode =
  (string "nop" $> Nop) <|>
  (string "acc" $> Acc) <|>
  (string "jmp" $> Jmp)

instruction :: Parser (Opcode, Int)
instruction = do
  o <- opcode
  skipSpace
  i <- signed decimal
  pure (o, i)

program :: Parser [(Opcode, Int)]
program = instruction `sepBy` char '\n'

markPc = do
  cur <- use pc
  visited %= Set.insert cur

advance :: Con ()
advance = markPc *> (pc += 1)

step :: Con ()
step = do
  is <- use instrs
  cur <- use pc
  let (o, v) = is !! cur
  step' o v

step' :: Opcode -> Int -> Con ()
step' Nop _ = advance
step' Acc x = (acc += x) *> advance
step' Jmp ix = markPc *> (pc += ix)

runToLoop :: Con Int
runToLoop = do
  cur <- use pc
  v <- use visited
  if Set.member cur v
     then use acc
     else step *> runToLoop

main :: IO ()
main = do
  input <- (\(Right x) -> x) . parseOnly program <$> T.readFile "inputs/8.txt"

  -- Part 1
  print . fst $ runCon input runToLoop
