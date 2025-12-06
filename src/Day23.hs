module Day23 where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt, intToDigit)
import Data.List (unfoldr)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as M
import Debug.Trace (traceShowM)
import Paths_AOC2020
import Data.Word (Word32)

input = "962713854"

test = "389125467"

initInput :: String -> Word32 -> ST s (M.STVector s Word32)
initInput s limit = do
  l <- M.generate (fromIntegral limit + 1) ((+ 1) . fromIntegral)
  f 0 s' l
  return l
  where
    s' = map (fromIntegral . digitToInt) s
    t = maximum s' + 1
    f i [] l =
      if maximum s' < limit
        then M.write l (fromIntegral i) t >> M.write l (fromIntegral limit) (head s')
        else M.write l (fromIntegral i) (head s')
    f i (x : xs) l = M.write l (fromIntegral i) x >> f x xs l

printV :: U.Vector Int -> [Int]
printV v = take (l - 1) $ unfoldr (\b -> Just (v U.! b, v U.! b)) 0
  where
    l = U.length v

step :: Word32 -> M.STVector s Word32 -> ST s ()
step limit v = do
  r <- M.read v 0
  h <- M.read v $ fromIntegral r
  m <- M.read v $ fromIntegral h
  t <- M.read v $ fromIntegral m
  r' <- M.read v $ fromIntegral t
  let f x
        | x <= 0 = f limit
        | x `elem` [h, m, t] = f (x - 1)
        | otherwise = x
      target = f (r - 1)
  afterTarget <- M.read v $ fromIntegral target
  M.write v 0 r'
  M.write v (fromIntegral r) r'
  M.write v (fromIntegral target) h
  M.write v (fromIntegral t) afterTarget

ans :: String -> Word32 -> Word32 -> Word32 -> [Word32]
ans input limit n takeN = runST $ do
  l <- initInput input limit
  forM_ [1 .. n] $ \_ -> step limit l
  let f 0 _ = return []
      f x y = do
        y' <- M.read l $ fromIntegral y
        (y' :) <$> f (x - 1) y'
  f takeN 1

day23 :: IO (String, String)
day23 = do
  let
   !finalAnsa
    = map (intToDigit . fromIntegral)
    $ ans input 9 100 8
  let
   !finalAnsb
    = show
    . product
    $ ans input 1000000 10000000 2
  pure (finalAnsa, finalAnsb)
