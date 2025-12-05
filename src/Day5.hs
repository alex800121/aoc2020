module Day5 where

import Data.List (foldl', sort, (\\))
import Paths_AOC2020

day5 :: IO (String, String)
day5 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  let seatIDs = sort $ map (foldl' (\acc x -> acc * 2 + if x `elem` "FL" then 0 else 1) 0) input
      b = maximum seatIDs
      a = minimum seatIDs
  let
   !finalAnsa
    = show
    $ maximum seatIDs
  let
   !finalAnsb
    = show
    $ [a .. b] \\ seatIDs
  pure (finalAnsa, finalAnsb)
