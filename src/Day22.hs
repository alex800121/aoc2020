module Day22 where

import Data.List.Split (splitOn)
import Data.Sequence
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)

type GameState = (Seq Int, Seq Int)

day22a :: GameState -> Int
day22a (lhs, rhs) = case (viewl lhs, viewl rhs) of
  (EmptyL, _) -> calc rhs
  (_, EmptyL) -> calc lhs
  (l :< ls, r :< rs) -> day22a $ if l > r then (ls :|> l :|> r, rs) else (ls, rs :|> r :|> l)

calc = snd . foldr (\x (i, acc) -> (i + 1, acc + (x * i))) (1, 0)

day22b :: Set GameState -> GameState -> Either Int Int
-- day22b _ g | traceShow g False = undefined
day22b cache g@(lhs, rhs) = case (viewl lhs, viewl rhs) of
  (EmptyL, _) -> Right $ calc rhs
  (_, EmptyL) -> Left $ calc lhs
  _ | g `Set.member` cache -> Left $ calc lhs
  (l :< ls, r :< rs)
    | l <= Seq.length ls,
      r <= Seq.length rs ->
        let lhs' = Seq.take l ls
            rhs' = Seq.take r rs
            result = case day22b Set.empty (lhs', rhs') of
              Left _ -> (ls :|> l :|> r, rs)
              Right _ -> (ls, rs :|> r :|> l)
         in day22b (Set.insert g cache) result
  (l :< ls, r :< rs) ->
    day22b (Set.insert g cache) $
      if l > r then (ls :|> l :|> r, rs) else (ls, rs :|> r :|> l)

day22 :: IO ()
day22 = do
  input <-
    (\(x : y : _) -> (x, y))
      . map (Seq.fromList . map (read @Int) . tail . lines)
      . splitOn "\n\n"
      -- <$> readFile "input/test22.txt"
      <$> readFile "input/input22.txt"
  putStrLn
    . ("day22a: "++)
    . show
    $ day22a input
  putStrLn
    . ("day22a: "++)
    . show
    $ day22b Set.empty input
