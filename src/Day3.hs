{-# LANGUAGE LambdaCase #-}

module Day3 where

import Data.Array.IArray
import Data.Bifunctor (bimap)
import Debug.Trace (traceShow)

type Index = (Int, Int)
type Forest = Array Index Bool

parseInput :: [String] -> Forest
parseInput s =
  array ((0, 0), (w - 1, h - 1)) $
    concat $
      zipWith (\y -> zipWith (\a b -> ((a, y), b == '#')) [0 ..]) [0 ..] s
  where
    w = length $ head s
    h = length s

collision :: Index -> Forest -> Index -> Int
collision start forest interval = length $ filter (forest !) coarse
  where
    (x, y) = bounds forest
    coarse = takeWhile ((<= snd y) . snd) $ iterate (bimap ((`mod` (fst y + 1)) . (+ fst interval)) (+ snd interval)) start

day3 :: IO ()
day3 = do
  input <- parseInput . lines <$> readFile "input/input3.txt"
  -- input <- parseInput . lines <$> readFile "input/test3.txt"
  putStrLn $ ("day3a: " ++) $ show $ collision (0, 0) input (3, 1)
  putStrLn $ ("day3b: " ++) $ show $ product $ map (collision (0, 0) input) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
