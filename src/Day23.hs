module Day23 where

import Data.Char (digitToInt)
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as U
import Control.Monad.ST (ST, runST)

input = "962713854"
test = "389125467"

initInput :: String -> Int -> ST s (M.STVector s Int)
initInput s limit = do
  l <- M.generate (limit + 1) id
  f 0 s' l
  M.write l limit (head s')
  return l
  where
  s' = map digitToInt s
  t = maximum s' + 1
  -- f :: Int -> [Int] -> ST s (M.MVector s Int) -> ST s () 
  f i [] l = M.write l i t
  f i (x : xs) l = M.write l i x >> f x xs l

step :: Int -> M.STVector s Int -> ST s (M.STVector s Int)
step limit v = do
  r <- M.read v 0
  h <- M.read v r
  x2 <- M.read v h >>= M.read v
  undefined

day23 :: IO ()
day23 = do
  putStrLn
    . ("day23a: " ++)
    . show
    $ ""
