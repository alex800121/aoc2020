{-# LANGUAGE ScopedTypeVariables #-}

module Day15 where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.List (find)
import MyLib (firstRepeat)
import Paths_AOC2020

input = [1, 0, 15, 2, 10, 13]

initArray :: [Int] -> Int -> ST s (STVector s Int)
initArray initList target = do
  a <- MV.replicate (target + 1) 0
  let l = init $ zip initList [1 ..]
  mapM_ (uncurry (MV.write a)) l
  return a

day15b' :: [Int] -> Int -> Int
day15b' initList target = runST $ do
  a <- initArray initList target
  let f i n
        | i == target = return n
        | otherwise = do
            x <- MV.read a n
            if x == 0
              then MV.write a n i >> f (i + 1) 0
              else MV.write a n i >> f (i + 1) (i - x)
  f (length initList) (last initList)

day15 :: IO ()
day15 = do
  putStrLn
    . ("day15a: " ++)
    . show
    $ day15b' input 2020
  putStrLn
    . ("day15b: " ++)
    . show
    $ day15b' input 30000000
