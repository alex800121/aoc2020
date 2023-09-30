module Day23 where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt, intToDigit)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Debug.Trace (traceShowM)

input = "962713854"

test = "389125467"

initInput :: String -> Int -> ST s (M.STVector s Int)
initInput s limit = do
  l <- M.generate (limit + 1) (+ 1)
  f 0 s' l
  return l
  where
    s' = map digitToInt s
    t = maximum s' + 1
    f i [] l = 
      if maximum s' < limit 
        then M.write l i t >> M.write l limit (head s')
        else M.write l i (head s')
    f i (x : xs) l = M.write l i x >> f x xs l

printV :: U.Vector Int -> [Int]
printV v = take (l - 1) $ unfoldr (\b -> Just (v U.! b, v U.! b)) 0
  where
    l = U.length v

step :: Int -> M.STVector s Int -> ST s ()
step limit v = do
  r <- M.read v 0
  h <- M.read v r
  m <- M.read v h
  t <- M.read v m
  r' <- M.read v t
  let f x
        | x <= 0 = f limit
        | x `elem` [h, m, t] = f (x - 1)
        | otherwise = x
      target = f (r - 1)
  afterTarget <- M.read v target
  M.write v 0 r'
  M.write v r r'
  M.write v target h
  M.write v t afterTarget

ans :: String -> Int -> Int -> Int -> [Int]
ans input limit n takeN = runST $ do
  l <- initInput input limit
  forM_ [1 .. n] $ \_ -> step limit l
  let f 0 _ = return []
      f x y = do
        y' <- M.read l y
        (y' :) <$> f (x - 1) y'
  f takeN 1

day23 :: IO ()
day23 = do
  putStrLn
    . ("day23a: " ++)
    . map intToDigit
    $ ans input 9 100 8
  putStrLn
    . ("day23b: " ++)
    . show
    . product
    $ ans input 1000000 10000000 2
