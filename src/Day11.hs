{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Day11 where

import Control.Monad (foldM)
import Control.Monad.ST.Strict (ST, runST)
import Control.Parallel (par)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict qualified as Map
import Data.Tuple (swap)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as M
import MyLib (drawArray)
import Paths_AOC2020

type Index = (Int, Int)

type Seats = UArray Index Char

type VSeats = UV.Vector Bool

type MVSeats s = M.STVector s Bool

type VNearest = V.Vector [Int]

surrounding :: [Index]
surrounding = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

run :: Int -> VNearest -> Int
run limit nearest = runST $ do
  seats0 <- M.replicate l False
  seats1 <- M.replicate l False
  f seats0 seats1
  where
    l = V.length nearest
    f :: MVSeats s -> MVSeats s -> ST s Int
    f s0 s1 = do
      acc' <-
        M.ifoldM'
          ( \acc i c -> do
              s <- foldM (\acc v -> M.read s0 v >>= \case True -> pure (succ acc); False -> pure acc) 0 (nearest V.! i)
              if
                | c && s >= limit -> M.write s1 i False >> pure (acc + 1)
                | not c && s == 0 -> M.write s1 i True >> pure (acc + 1)
                | otherwise -> M.write s1 i c >> pure acc
          )
          0
          s0
      if acc' == 0 then M.foldl' (\acc x -> if x then succ acc else acc) 0 s0 else f s1 s0

step :: Int -> VNearest -> VSeats -> VSeats
step limit nearest seats = UV.imap f seats
  where
    f i c
      | c && s >= limit = False
      | not c && s == 0 = True
      | otherwise = c
      where
        s = length $ filter (seats UV.!) $ nearest V.! i

nearestA :: Seats -> VNearest
nearestA s = vnearest
  where
    vMap = Map.fromList $ zip [i | (i, 'L') <- A.assocs s] [0 ..]
    l = Map.size vMap
    vnearest =
      V.accum
        (flip (:))
        (V.replicate l [])
        [ (vMap Map.! i, vMap Map.! j)
          | (i, 'L') <- A.assocs s,
            (x, y) <- surrounding,
            let j = bimap (+ x) (+ y) i,
            Just 'L' == (s A.!? j)
        ]

nearestB :: Seats -> VNearest
nearestB s = vnearest
  where
    vMap = Map.fromList $ zip [i | (i, 'L') <- A.assocs s] [0 ..]
    l = Map.size vMap
    vnearest =
      V.accum
        (flip (:))
        (V.replicate l [])
        [ (vMap Map.! i, vMap Map.! j)
          | (i, 'L') <- A.assocs s,
            (x, y) <- surrounding,
            j <- f x y (bimap (+ x) (+ y) i)
        ]
    f x y n = case s A.!? n of
      Nothing -> []
      Just 'L' -> [n]
      _ -> f x y (bimap (+ x) (+ y) n)

stablized :: (Eq a) => (a -> a) -> a -> (Int, a)
stablized = go 0
  where
    go i f x
      | x' == x = (i, x)
      | otherwise = go (succ i) f x'
      where
        x' = f x

day11 :: IO ()
day11 = do
  input <- drawArray @UArray . lines <$> (getDataDir >>= readFile . (++ "/input/input11.txt"))
  let a = par b $ nearestA input
      b = nearestB input
      l = V.length a
      -- ansA = par ansB $ run 4 a
      -- ansB = run 5 b
      ansA = par ansB $ UV.length . UV.filter id . snd $ stablized (step 4 a) (UV.replicate l False)
      ansB = UV.length . UV.filter id . snd $ stablized (step 5 b) (UV.replicate l False)
  putStrLn
    . ("day11a: " ++)
    . show
    $ ansA
  putStrLn
    . ("day11b: " ++)
    . show
    $ ansB
