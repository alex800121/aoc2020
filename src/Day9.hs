module Day9 where


import Paths_AOC2020
import Control.Monad (guard, join)

import Data.List (find, permutations, sort, tails, uncons, (\\))

import Data.List.Split (divvy)

import Data.Maybe (mapMaybe)

import Debug.Trace (traceShow)

import MyLib (pick)

targetSum :: (Num a, Ord a) => a -> [a] -> Maybe [a]
targetSum target source = go (target - head source) 0 0
  where
    go x i j
      | j >= length source || i > j = Nothing
      | x == 0 = Just $ map (source !!) [i .. j]
      | x < 0 = go (x + source !! i) (i + 1) j
      | x > 0 = go (x - source !! (j + 1)) i (j + 1)

day9 :: IO ()
day9 = do
  input <- map (read @Int) . lines <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  let xs = init $ divvy 25 1 input
      x = drop 25 input
      day9a = fmap fst $ find (not . snd) $ zipWith (\a b -> (a, a `elem` map sum (filter ((/=) <$> head <*> (!! 1)) $ pick 2 b))) x xs
  putStrLn $ ("day9a: " ++) $ show day9a
  putStrLn $ ("day9b: " ++) $ show $ fmap ((+) <$> minimum <*> maximum) $ join $ targetSum <$> day9a <*> pure input
