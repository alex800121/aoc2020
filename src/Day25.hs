module Day25 where

import Data.List (elemIndex, find, findIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceShow)

doorPub = 1526110

cardPub = 20175123

d = 20201227

subject = 7

f pubNum subNum divider = (pubNum * subNum) `mod` divider

g i n target subject
  | n == target = i
  | otherwise = g (i + 1) (f n subject d) target subject

h i n subject
  | i <= 0 = n
  | otherwise = h (i - 1) (f n subject d) subject

day25 :: IO ()
day25 = do
  let door = g 0 1 doorPub subject 
  putStrLn . ("day25a: " ++) . show $ h door 1 cardPub
