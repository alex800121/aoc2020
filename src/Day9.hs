module Day9 where

import Data.List.Split (divvy)
import MyLib (pick)
import Data.List (find, permutations, (\\), sort, uncons, tails)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

targetSum :: (Num a, Ord a) => a -> [a] -> [[a]]
targetSum target source = go target (reverse $ sort source)
  where
    go t s
      | null s || t < 0 = []
      | t == 0 = [[]]
      | otherwise = do
          (x, xs) <- mapMaybe uncons $ tails s
          guard $ x <= t
          (x :) <$> go (t - x) xs

day9 :: IO ()
day9 = do
  input <- map (read @Int) . lines <$> readFile "input/input9.txt"
  let xs = init $ divvy 25 1 input
      x = drop 25 input
      day9a = fmap fst $ find (not . snd) $ zipWith (\a b -> (a, a `elem` map sum (filter ((/=) <$> head <*> (!! 1)) $ pick 2 b))) x xs
  putStrLn $ ("day9a: " ++) $ show day9a
  putStrLn $ ("day9b: " ++) $ show $ fmap (((+) <$> maximum <*> minimum) . head) $ targetSum <$> day9a <*> pure (filter ((< day9a) . Just) input)
