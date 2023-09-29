module Day22 where

import Data.List.Split (splitOn)
import Data.Sequence
import qualified Data.Sequence as Seq

type GameState = (Seq Int, Seq Int)

day22a :: GameState -> Int
day22a (lhs, rhs) = case (viewl lhs, viewl rhs) of
  (EmptyL, _) -> calc rhs
  (_ , EmptyL) -> calc lhs
  (l :< ls, r :< rs) -> day22a $ if l > r then (ls :|> l :|> r, rs) else (ls, rs :|> r :|> l)
  where
    calc = snd . foldr (\x (i, acc) -> (i + 1, acc + (x * i))) (1, 0)

day22 :: IO ()
day22 = do
  input <-
    (\(x : y : _) -> (x, y))
      . map (Seq.fromList . map (read @Int) . tail . lines)
      . splitOn "\n\n"
      -- <$> readFile "input/test22.txt"
      <$> readFile "input/input22.txt"
  print $ day22a input
