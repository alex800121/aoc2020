module Day6 where

import Data.Set qualified as Set
import Paths_AOC2020

import Data.List.Split (splitOn)

import Data.List (foldl1', intersect)

day6 :: IO (String, String)
day6 = do
  input <- map lines . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  let
   !finalAnsa
    = show
    $ sum
    $ map (Set.size . Set.fromList . concat) input
  let
   !finalAnsb
    = show
    $ sum
    $ map (length . foldl1' intersect) input
  pure (finalAnsa, finalAnsb)
