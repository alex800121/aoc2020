{-# LANGUAGE LambdaCase #-}

module Day17 where

import Data.Array.IArray qualified as I
import Data.Array.Unboxed qualified as U
-- import Data.Array.MArray qualified as M
-- import Data.Array.ST (STUArray)
import Data.List (groupBy, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import MyLib (drawGraph, drawMap)
import Paths_AOC2020

type Index = (Int, Int, Int)

type Cube = U.UArray Index Bool

type HyperIndex = (Int, Int, Int, Int)

type HyperCube = U.UArray HyperIndex Bool


adjacent :: [Index]
adjacent = [(a, b, c) | a <- [-1 .. 1], b <- [-1 .. 1], c <- [-1 .. 1], (a, b, c) /= (0, 0, 0)]

hyperAdjacent :: [HyperIndex]
hyperAdjacent = [(a, b, c, d) | a <- [-1 .. 1], b <- [-1 .. 1], c <- [-1 .. 1], d <- [-1 .. 1], (a, b, c, d) /= (0, 0, 0, 0)]

(+^) :: Index -> Index -> Index
(a, b, c) +^ (d, e, f) = (a + d, b + e, c + f)

(+^^) :: HyperIndex -> HyperIndex -> HyperIndex
(a, b, c, x) +^^ (d, e, f, y) = (a + d, b + e, c + f, x + y)

hyperStep :: HyperCube -> HyperCube
hyperStep c = I.genArray b' f
  where
    b@((x0, y0, z0, w0), (x1, y1, z1, w1)) = U.bounds c
    b' = ((x0 - 1, y0 - 1, z0 - 1, w0 - 1), (x1 + 1, y1 + 1, z1 + 1, w1 + 1))
    f i = if fromMaybe False (c I.!? i) then l == 2 || l == 3 else l == 3
      where
        i' = map (+^^ i) hyperAdjacent
        l = length $ filter (fromMaybe False . (c I.!?)) i'

step :: Cube -> Cube
step c = I.genArray b' f
  where
    b@((x0, y0, z0), (x1, y1, z1)) = U.bounds c
    b' = ((x0 - 1, y0 - 1, z0 - 1), (x1 + 1, y1 + 1, z1 + 1))
    f i = if fromMaybe False (c I.!? i) then l == 2 || l == 3 else l == 3
      where
        i' = map (+^ i) adjacent
        l = length $ filter (fromMaybe False . (c I.!?)) i'

printCube :: Cube -> [String]
printCube =
  map
    ( unlines
        . drawGraph (\case Nothing -> ' '; Just True -> '#'; Just False -> '.')
        . Map.fromList
        . map (\((x, y, _), e) -> ((x, y), e))
    )
    . groupBy (\((_, _, z), _) ((_, _, c), _) -> z == c)
    . sortBy (\((_, _, z), _) ((_, _, c), _) -> compare z c)
    . U.assocs

day17 :: IO ()
day17 = do
  -- input <- lines <$> readFile "input/test17.txt"
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  let initMap =
        map (\((x, y), e) -> ((x, y, 0), e))
          . Map.toList
          $ drawMap (\case '#' -> Just True; '.' -> Just False; _ -> Nothing) input
      b@((x0, y0, z0), (x1, y1, z1)) = (,) <$> minimum <*> maximum $ map fst initMap
      initArray = U.array b initMap :: Cube
      initHyperArray =
        U.array
          ((x0, y0, z0, 0), (x1, y1, z1, 0))
          (map (\((x, y, z), e) -> ((x, y, z, 0), e)) initMap) ::
          HyperCube
  putStrLn
    . ("day17a: " ++)
    . show
    . length
    . filter id
    . U.elems
    . (!! 6)
    $ iterate step initArray
  putStrLn
    . ("day17b: " ++)
    . show
    . length
    . filter id
    . U.elems
    . (!! 6)
    $ iterate hyperStep initHyperArray
