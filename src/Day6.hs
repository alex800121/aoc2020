module Day6 where


import Paths_AOC2020
import qualified Data.Set as Set

import Data.List.Split (splitOn)

import Data.List (foldl1', intersect)
day6 :: IO ()
day6 = do
  input <- map lines . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  putStrLn $ ("day6a: " ++) $ show $ sum $ map (Set.size . Set.fromList . concat) input
  putStrLn $ ("day6b: " ++) $ show $ sum $ map (length . foldl1' intersect) input
