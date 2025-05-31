module Day24 where

import Control.Monad (foldM)
import Control.Monad.ST.Strict (runST)
import Data.Array.IArray qualified as I
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.IntSet qualified as IS
import Data.List (foldl', nub, sort)
import Data.Maybe (mapMaybe)
import Data.MultiSet qualified as MS
import Data.Vector.Unboxed.Mutable qualified as MV
import Debug.Trace
import Paths_AOC2020

type Index = (Int, Int)

parseDirection :: Index -> String -> Index
parseDirection i [] = i
parseDirection (x, y) ('e' : xs) = parseDirection (x + 1, y) xs
parseDirection (x, y) ('s' : 'e' : xs) = parseDirection (x + 1, y + 1) xs
parseDirection (x, y) ('s' : 'w' : xs) = parseDirection (x, y + 1) xs
parseDirection (x, y) ('w' : xs) = parseDirection (x - 1, y) xs
parseDirection (x, y) ('n' : 'w' : xs) = parseDirection (x - 1, y - 1) xs
parseDirection (x, y) ('n' : 'e' : xs) = parseDirection (x, y - 1) xs

adjacent = [(1, 0), (1, 1), (0, 1), (-1, 0), (-1, -1), (0, -1)]

{-

##
###
 ##

-}

toInt (x, y) = x * (gen * 2 + len) + y

gen = 101

len = 27

step :: UArray Index Bool -> UArray Index Bool
step a = I.accumArray (const id) False b' [(i, f i b) | (i, b) <- I.assocs a]
  where
    b = I.bounds a
    b' = bimap (bimap pred pred) (bimap succ succ) b
    f (x, y) b = b && s == 1 || s == 2 || not b && s == 2
      where
        s = length $ filter id $ mapMaybe ((a I.!?) . bimap (+ x) (+ y)) adjacent

run :: [Index] -> Int -> Int
run xs i = length $ filter id $ I.elems $ iterate step a !! i
  where
    b = foldl' (\((a, b), (c, d)) (x, y) -> ((min a x, min b y), (max c x, max d y))) ((0, 0), (0, 0)) xs
    b' = bimap (bimap pred pred) (bimap succ succ) b
    a = I.accumArray (const id) False b' [(i, True) | i <- xs]

run' :: [Index] -> Int -> Int
run' xs i = runST $ do
  length
    <$> foldM
      ( \xs _ -> do
          s <- MV.replicate (len * len) (0 :: Int)
          c <-
            foldM
              ( \acc x -> do
                  foldM
                    ( \a b -> do
                        MV.modify s succ (x + b)
                        n <- MV.read s (x + b)
                        if n == 2 then pure ((x + b) : a) else pure a
                    )
                    acc
                    iadjacent
              )
              []
              xs
          o0 <- foldM (\a b -> MV.read s b >>= \x -> if x == 1 || x == 2 then pure (IS.insert b a) else pure a) IS.empty xs
          IS.toList <$> foldM (\a b -> MV.read s b >>= \x -> if x == 2 then pure (IS.insert b a) else pure a) o0 c
      )
      (IS.toList xs')
      [0 .. i - 1]
  where
    b@((minx, miny), (maxx, maxy)) = foldl' (\((a, b), (c, d)) (x, y) -> ((min a x, min b y), (max c x, max d y))) ((0, 0), (0, 0)) xs
    len = (maxy - miny + 1) + (i * 2) + 2
    toInt (x, y) = x * len + y
    xs' = IS.fromList $ map (toInt . bimap (+ (i + minx)) (+ (i + miny))) xs
    iadjacent = map toInt adjacent

day24 :: IO ()
day24 = do
  input <-
    MS.foldOccur (\x o acc -> if odd o then x : acc else acc) []
      . MS.fromList
      . map (parseDirection (0, 0))
      . lines
      <$> (getDataDir >>= readFile . (++ "/input/input24.txt"))
  putStrLn
    . ("day24a: " ++)
    . show
    . length
    $ input
  putStrLn
    . ("day24b: " ++)
    . show
    $ run' input 100
