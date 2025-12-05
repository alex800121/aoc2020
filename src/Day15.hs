module Day15 where

import Control.Monad.ST.Strict (ST, runST)
import Data.Bits (Bits (..))
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word32)
import Paths_AOC2020
import Data.Bifunctor (Bifunctor(..))

input = [1, 0, 15, 2, 10, 13]

initArray :: ST s (STVector s Word32)
initArray = do
  a <- MV.replicate 30000000 0
  let l = init $ zip input [1 ..]
  mapM_ (uncurry (MV.write a) . first fromIntegral ) l
  pure a

day15b' t0 = runST $ do
  a <- initArray
  z <- MV.read a 0
  f (fromIntegral $ length input) (last input) z a
  where
    f :: Word32 -> Word32 -> Word32 -> STVector s Word32 -> ST s Word32
    f i n zero a
      | n == 0 = f (i + 1) (i - zero) i a
      | i == t0 = return n
      | otherwise = do
          x <- MV.unsafeExchange a (fromIntegral n) i
          if x == 0
            then f (i + 1) 0 zero a
            else f (i + 1) (i - x) zero a

day15 :: IO (String, String)
day15 = do
  let
   !finalAnsa
    = show
    $ day15b' 2020
  let
   !finalAnsb
    = show
    $ day15b' 30000000
  pure (finalAnsa, finalAnsb)
