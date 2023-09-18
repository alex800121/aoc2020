module Day1 where

import Data.List
import Data.Maybe (maybeToList, mapMaybe)
import Control.Applicative (empty)
import Control.Monad (guard)

findTarget :: Int -> Int -> [Int] -> [[Int]]
findTarget target n l | n == 0 = if target == 0 then pure [] else empty
findTarget target n l = do
  (x, xs) <- mapMaybe uncons $ tails l
  guard $ x <= target
  (x :) <$> findTarget (target - x) (n - 1) xs

day1 :: IO ()
day1 = do
  input <- sort . map (read @Int) . lines <$> readFile "input/input1.txt"
  putStrLn $ ("day1a: " ++) $ show $ map product $ findTarget 2020 2 input
  putStrLn $ ("day1b: " ++) $ show $ map product $ findTarget 2020 3 input
