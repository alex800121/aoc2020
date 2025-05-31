module Day22 where

import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.Hashable
import Data.List.Split (splitOn)
import Data.Sequence (Seq (..), ViewL (..), viewl)
import Data.Sequence qualified as Seq
import Debug.Trace (traceShow)
import Paths_AOC2020

type GameState = (Seq Int, Seq Int)

day22a :: GameState -> Int
day22a (lhs, rhs) = case (viewl lhs, viewl rhs) of
  (EmptyL, _) -> calc rhs
  (_, EmptyL) -> calc lhs
  (l :< ls, r :< rs) -> day22a $ if l > r then (ls :|> l :|> r, rs) else (ls, rs :|> r :|> l)

calc = snd . foldr (\x (i, acc) -> (i + 1, acc + (x * i))) (1, 0)

day22b :: Int -> HashSet GameState -> GameState -> Either Int Int
day22b level cache g@(lhs, rhs) = case (viewl lhs, viewl rhs) of
  (EmptyL, _) -> Right $ calc rhs
  (_, EmptyL) -> Left $ calc lhs
  _ | g `Set.member` cache -> Left $ calc lhs
  _ | level /= 0 && lHigh > rHigh && lHigh > len - 2 -> Left 0
  (l :< ls, r :< rs)
    | l <= Seq.length ls,
      r <= Seq.length rs ->
        let lhs' = Seq.take l ls
            rhs' = Seq.take r rs
            result = case day22b (succ level) Set.empty (lhs', rhs') of
              Left _ -> (ls :|> l :|> r, rs)
              Right _ -> (ls, rs :|> r :|> l)
         in day22b level (Set.insert g cache) result
  (l :< ls, r :< rs) ->
    day22b level (Set.insert g cache) $
      if l > r then (ls :|> l :|> r, rs) else (ls, rs :|> r :|> l)
  where
    lHigh = maximum lhs
    rHigh = maximum rhs
    len = length lhs + length rhs

day22 :: IO ()
day22 = do
  input <-
    (\(x : y : _) -> (x, y))
      . map (Seq.fromList . map (read @Int) . tail . lines)
      . splitOn "\n\n"
      <$> (getDataDir >>= readFile . (++ "/input/input22.txt"))
  putStrLn
    . ("day22a: " ++)
    . show
    $ day22a input
  putStrLn
    . ("day22b: " ++)
    . show
    $ day22b 0 Set.empty input
