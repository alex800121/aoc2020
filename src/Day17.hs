{-# LANGUAGE LambdaCase #-}

module Day17 where

import Control.Monad (replicateM_, when)
import Control.Monad.ST.Strict (ST, runST)
import Data.IntMultiSet qualified as IMS
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word8)
import MyLib (drawMap)
import Paths_AOC2020

type Index = (Int, Int, Int)

type HyperIndex = (Int, Int, Int, Int)

fac = 8 + n * 2

fromHyperIndex (x, y, z, w) = x * fac ^ 3 + y * fac ^ 2 + z * fac + w

fromIndex (x, y, z) = x * fac ^ 2 + y * fac + z

nextIMS f n y here = IMS.foldOccur p IS.empty adj
  where
    here' = IS.mapMonotonic (+ f y) here
    adj = IMS.unions $ map (\x -> IMS.fromSet $ IS.mapMonotonic (+ f x) here) n
    p k n acc = if n == 3 || n == 2 && IS.member k here' then IS.insert k acc else acc

adjacent :: [Index]
adjacent = [(a, b, c) | a <- [0 .. 2], b <- [0 .. 2], c <- [0 .. 2], (a, b, c) /= (1, 1, 1)]

hyperAdjacent :: [HyperIndex]
hyperAdjacent = [(a, b, c, d) | a <- [0 .. 2], b <- [0 .. 2], c <- [0 .. 2], d <- [0 .. 2], (a, b, c, d) /= (1, 1, 1, 1)]

(+^) :: Index -> Index -> Index
(a, b, c) +^ (d, e, f) = (a + d, b + e, c + f)

(+^^) :: HyperIndex -> HyperIndex -> HyperIndex
(a, b, c, x) +^^ (d, e, f, y) = (a + d, b + e, c + f, x + y)

hyperStep :: (a -> Int) -> [a] -> a -> STVector s Word8 -> STVector s Bool -> STVector s Bool -> ST s ()
hyperStep f adj z count c' d' = do
  MV.set count 0
  MV.set c' False
  MV.imapM_
    ( \i a ->
        when a (mapM_ (\j -> MV.modify count (+ 1) (f j + i)) adj >> MV.write c' (f z + i) True)
    )
    d'
  MV.set d' False
  MV.imapM_
    ( \i n -> do
        x <- MV.read c' i
        when (n == 3 || n == 2 && x) (MV.write d' i True)
    )
    count
  where
    l = MV.length c'

n = 6

day17a m l f xs z = runST $ do
  d' <- MV.generate l (`IS.member` m)
  c' <- MV.new l
  count <- MV.new l
  replicateM_ n (hyperStep f xs z count c' d')
  MV.foldl' (\acc x -> if x then acc + 1 else acc) 0 d'

day17 :: IO (String, String)
day17 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  let initM =
        IS.fromList
          . map (\(x, y) -> fromIndex (x, y, 0))
          . Map.keys
          $ drawMap (\case '#' -> Just (); _ -> Nothing) input
      initHyperM = IS.map (* fac) initM
  let
   !finalAnsa
    = show
    $ day17a initM (fac ^ 3) fromIndex adjacent (1, 1, 1)
  let
   !finalAnsb
    = show
    $ day17a initHyperM (fac ^ 4) fromHyperIndex hyperAdjacent (1, 1, 1, 1)
  pure (finalAnsa, finalAnsb)
