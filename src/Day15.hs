{-# LANGUAGE ScopedTypeVariables #-}

module Day15 where


import Paths_AOC2020
import Control.Monad (forM_)

import Control.Monad.ST (ST, runST)

import Data.Array.Base (STUArray (STUArray))

import Data.Array.MArray

import Data.Array.ST (STUArray)

import Data.IntMap.Strict (IntMap)

import qualified Data.IntMap.Strict as IntMap

import Data.List (find)

import MyLib (firstRepeat)

type Mem = ((Int, Int), IntMap Int)

input = [1, 0, 15, 2, 10, 13]

initMem :: Mem
initMem = ((last input, length input), IntMap.fromList $ zip (init input) [1 ..])

step :: Mem -> Mem
step ((n, i), m) = case m IntMap.!? n of
  Just x -> ((i - x, i + 1), IntMap.insert n i m)
  _ -> ((0, i + 1), IntMap.insert n i m)

day15b :: Int -> Mem -> Int
day15b target mem = if target == snd (fst mem) then fst (fst mem) else day15b target (step mem)

initArray :: [Int] -> Int -> ST s (STUArray s Int Int)
initArray initList target = do
  a <- newArray (0, target) 0
  let l = init $ zip initList [1 ..]
  mapM_ (uncurry (writeArray a)) l
  return a

day15b' :: [Int] -> Int -> Int
day15b' initList target = runST $ do
  a <- initArray initList target
  let f i n
        | i == target = return n
        | otherwise = do
            x <- readArray a n
            if x == 0
              then writeArray a n i >> f (i + 1) 0
              else writeArray a n i >> f (i + 1) (i - x)
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
