module Day14 where

import Data.List (foldl', unfoldr)
import Data.Tuple (swap)

type Mask = [Maybe Bool]

type Bit = [Bool]

bitToDec :: (Integral a) => Bit -> a
bitToDec = foldl' (\acc x -> acc * 2 + if x then 1 else 0) 0

decToBit :: Word -> Bit
decToBit = go []
  where
    go = undefined

day14 :: IO ()
day14 = do
  print $ decToBit 10
