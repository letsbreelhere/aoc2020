{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskellQuotes #-}

module Main where

import Data.Maybe (catMaybes)
import Data.List (find)
import Control.Lens ((%=), (.=), (+=), (^.), makeLenses, use)
import Control.Monad.State
import Control.Applicative
import Data.Functor
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Attoparsec.Text hiding (take)
import qualified Data.Text.IO as T

data Opcode = Nop
            | Acc
            | Jmp
            deriving (Show)

data ConState = ConState { _instrs :: [(Opcode, Int)], _acc :: Int, _pc :: Int, _visited :: Set Int, _done :: Bool }
makeLenses ''ConState

newtype Con a = Con { unCon :: State ConState a }
  deriving (Functor, Applicative, Monad, MonadState ConState)

runCon' :: Con a -> ConState -> (a, ConState)
runCon' = runState . unCon

runCon :: [(Opcode, Int)] -> Con a -> (a, ConState)
runCon is c = runCon' c (ConState is 0 0 Set.empty False)

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
  if cur == length is
     then done .= True
     else let (o, v) = is !! cur in step' o v

step' :: Opcode -> Int -> Con ()
step' Nop _ = advance
step' Acc x = (acc += x) *> advance
step' Jmp ix = markPc *> (pc += ix)

runToLoop :: Con Int
runToLoop = do
  d <- use done
  cur <- use pc
  v <- use visited
  if d || Set.member cur v
     then use acc
     else step *> runToLoop

terminates :: [(Opcode, Int)] -> Bool
terminates ops =
  let (_, res) = runCon ops runToLoop
   in res ^. done

zippers :: [a] -> [([a], a, [a])]
zippers as =
  let withIndex = as `zip` [0..]
   in map (\(a, ix) -> (take ix as, a, drop (ix+1) as)) withIndex

zipperMap :: ([a] -> a -> [a] -> b) -> [a] -> [b]
zipperMap f = map (\(ls, a, rs) -> f ls a rs) . zippers

trySwap ls (Nop, i) rs = Just (ls ++ [(Jmp, i)] ++ rs)
trySwap ls (Jmp, i) rs = Just (ls ++ [(Nop, i)] ++ rs)
trySwap _ _ _ = Nothing

main :: IO ()
main = do
  input <- (\(Right x) -> x) . parseOnly program <$> T.readFile "inputs/8.txt"

  -- Part 1
  print . fst $ runCon input runToLoop

  let swaps = catMaybes $ zipperMap trySwap input
      Just soln = find terminates swaps

  print . fst $ runCon soln runToLoop
