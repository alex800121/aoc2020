module Day8 where

import Control.Monad (foldM)
import Data.Either (rights)
import Data.Function (on)
import Data.IntSet qualified as IS
import Data.List (find, unfoldr)
import Data.Maybe (isNothing)
import Data.Vector qualified as V
import Paths_AOC2020

data Instruction a
  = Acc a
  | Jmp a
  | Nop a
  deriving (Show, Eq, Ord)

insParser :: String -> Instruction Int
insParser s = case words s of
  "acc" : x : _ -> Acc $ f x
  "jmp" : x : _ -> Jmp $ f x
  "nop" : x : _ -> Nop $ f x
  where
    f = read @Int . filter (/= '+')

run :: V.Vector (Instruction Int) -> Either Int Int
run v = go IS.empty 0 0
  where
    go visited i acc
      | i `IS.member` visited = Left acc
    go visited i acc = case v V.!? i of
      Nothing -> Right acc
      Just (Acc a) -> go visited' (succ i) (acc + a)
      Just (Jmp a) -> go visited' (i + a) acc
      Just (Nop a) -> go visited' (succ i) acc
      where
        visited' = IS.insert i visited

fix :: V.Vector (Instruction Int) -> [V.Vector (Instruction Int)]
fix v = [v V.// [(i, f j)] | (i, j) <- V.toList (V.indexed v)]
  where
    f (Acc a) = Acc a
    f (Jmp a) = Nop a
    f (Nop a) = Jmp a

day8 :: IO ()
day8 = do
  input <- V.fromList . map insParser . lines <$> (getDataDir >>= readFile . (++ "/input/input8.txt"))
  putStrLn
    . ("day8a: " ++)
    . show
    $ run input
  putStrLn
    . ("day8b: " ++)
    . show
    . rights
    . map run
    $ fix input
