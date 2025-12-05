module Day1 where

import Control.Applicative (empty)
import Control.Monad (guard)
import Data.List
import Data.Maybe (mapMaybe, maybeToList)
import Paths_AOC2020

findTarget :: Int -> Int -> [Int] -> [[Int]]
findTarget target n l | n == 0 = if target == 0 then pure [] else empty
findTarget target n l = do
  (x, xs) <- mapMaybe uncons $ tails l
  guard $ x <= target
  (x :) <$> findTarget (target - x) (n - 1) xs

day1 :: IO (String, String)
day1 = do
  input <- sort . map (read @Int) . lines <$> (getDataDir >>= readFile . (++ "/input/input1.txt"))
  let
   !finalAnsa
    = show
    $ map product
    $ findTarget 2020 2 input
  let
   !finalAnsb
    = show
    $ map product
    $ findTarget 2020 3 input
  pure (finalAnsa, finalAnsb)
