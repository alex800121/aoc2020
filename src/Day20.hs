{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day20 where


import Paths_AOC2020
import Control.Monad (guard)

import Data.Array.IArray

import Data.Array.Unboxed

import Data.Bifunctor (bimap)

import Data.Char (isNumber)

import Data.Either (partitionEithers)

import Data.Function ((&))

import Data.List (delete, find, foldl', nub, (\\))

import Data.List.Split (splitOn)

import Data.Map (Map)

import qualified Data.Map as Map

import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)

import Data.Set (Set)

import qualified Data.Set as Set

import Data.Tuple (swap)

import Debug.Trace (traceShow)

import MyLib (Direction (..), drawGraph, drawMap)

type PicMap = Map Index Tile

type Tile = UArray Index Bool

data TileType = Corner | Side | Center deriving (Show, Eq, Ord)

type TileVar = (Op' Int, Tile)

type Index = (Int, Int)

data Op = Flip | Rotate deriving (Show, Eq, Ord)

data Op' a = Op {flipN :: a, rotateN :: a} deriving (Show, Eq, Ord, Functor)

dragon :: Tile
dragon = array b $ Map.assocs m
  where
    b = (,) <$> minimum <*> maximum $ Map.keys m
    m =
      drawMap (\case '#' -> Just True; _ -> Just False)
        . lines
        $ "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "

findDragon :: Tile -> Tile -> [Index]
findDragon d t =
  [ (x, y)
    | let l =
            [ (a, b)
              | a <- [dx0 .. dx1],
                b <- [dy0 .. dy1]
            ],
      x <- [tx0 .. tx1 - dx1],
      y <- [ty0 .. ty1 - dy1],
      all (\(i, j) -> f (d ! (i, j)) (t ! (i - dx0 + x, j - dy0 + y))) l
  ]
  where
    ((dx0, dy0), (dx1, dy1)) = bounds d
    ((tx0, ty0), (tx1, ty1)) = bounds t
    f True a = a
    f False _ = True

normalize :: Tile -> Tile
normalize t = ixmap ((0, 0), (x1 - x0, y1 - y0)) (\(x, y) -> (x + x0, y + y0)) t
  where
    ((x0, y0), (x1, y1)) = bounds t

stitch :: PicMap -> Tile
stitch m =
  normalize $
    array
      ((cx + 1 + (ax * w), cy + 1 + (ay * h)), (dx - 1 + (bx * w), dy - 1 + (by * h)))
      [ ((x + (i * w), y + (j * h)), (m Map.! (i, j)) ! (x, y))
        | x <- [cx + 1 .. dx - 1],
          y <- [cy + 1 .. dy - 1],
          i <- [ax .. bx],
          j <- [ay .. by]
      ]
  where
    ((ax, ay), (bx, by)) = (,) <$> minimum <*> maximum $ Map.keys m
    ((cx, cy), (dx, dy)) = bounds $ m Map.! (ax, ay)
    bax = bx - ax + 1
    bay = by - ay + 1
    w = dx - cx - 1
    h = dy - cy - 1

picMap :: [Tile] -> PicMap
picMap [] = Map.empty
picMap (x : xs) = go Map.empty (Set.singleton ((0, 0), x)) xs
  where
    go :: PicMap -> Set (Index, Tile) -> [Tile] -> PicMap
    -- go m t ts | traceShow (Map.keys m) False = undefined
    go m t ts = case ts of
      [] -> m'
      tss ->
        let (ts', t') = fmap Set.fromList $ partitionEithers $ map (f (Set.toList t)) tss
            f [] a = Left a
            f (((xb, yb), b) : bs) a = case sharedSides b a of
              Nothing -> f bs a
              Just (a', (xa, ya)) -> Right ((xa + xb, ya + yb), a')
         in go m' t' ts'
      where
        m' = Set.foldr (uncurry Map.insert) m t

isTypeIn :: Tile -> [Tile] -> TileType
isTypeIn t ts = case s of
  2 -> Corner
  3 -> Side
  4 -> Center
  _ -> error "not possible"
  where
    ts' = delete t ts
    s = length $ mapMaybe (sharedSides t) ts'

tileVar :: Tile -> [TileVar]
tileVar t = matchT
  where
    opList = [Op x y | x <- [0, 1], y <- [0 .. 3]]
    matchT = map (\x -> (x, op x t)) opList

sharedSides :: Tile -> Tile -> Maybe (Tile, Index)
sharedSides ref t | ref == t = Nothing
sharedSides ref t = matchSide
  where
    rVar = tileVar ref
    matchSide = listToMaybe $ do
      (rOp, r') <- rVar
      (rDir, rSide) <- sides r'
      (tDir, tSide) <- sides t
      guard $ rSide == reverse tSide && tDir == succ (succ rDir)
      let rIndex = case rDir of
            North -> (0, -1)
            East -> (1, 0)
            South -> (0, 1)
            West -> (-1, 0)
      return (reverseOp rOp t, reverseIndex rOp rIndex)

reverseIndex :: Op' Int -> Index -> Index
reverseIndex (Op a b) i =
  foldr
    ($)
    i
    (replicate (a `mod` 2) flipIndex <> replicate (negate b `mod` 4) rotateIndex)
  where
    flipIndex (x, y) = (-x, y)
    rotateIndex (x, y) = (-y, x)

op, reverseOp :: Op' Int -> Tile -> Tile
op (Op a b) t =
  foldr
    ($)
    t
    (replicate (b `mod` 4) rotateTile <> replicate (a `mod` 2) flipTile)
reverseOp (Op a b) t =
  foldr
    ($)
    t
    (replicate (a `mod` 2) flipTile <> replicate (negate b `mod` 4) rotateTile)

sides :: Tile -> [(Direction, [Bool])]
sides t =
  map
    (fmap (map (t !)))
    [ (North, [(x, minY) | x <- [minX .. maxX]]),
      (East, [(maxX, y) | y <- [minY .. maxY]]),
      (South, [(x, maxY) | x <- reverse [minX .. maxX]]),
      (West, [(minX, y) | y <- reverse [minY .. maxY]])
    ]
  where
    ((minX, minY), (maxX, maxY)) = bounds t

flipDiagonal :: Tile -> Tile
flipDiagonal = rotateTile . flipTile

flipTile :: Tile -> Tile
flipTile t =
  let i = bounds t
   in ixmap
        i
        ( \(x, y) -> (negate (x - fst (snd i) + fst (fst i)), y)
        )
        t

rotateTile :: Tile -> Tile
rotateTile t =
  let i = bounds t
      i' = bimap swap swap i
   in ixmap
        i'
        ( \(x, y) ->
            (y, negate (x - fst (snd i') + fst (fst i')))
        )
        t

testTile :: Tile
testTile =
  array
    ((0, 0), (2, 1))
    [ ((0, 0), True),
      ((0, 1), False),
      ((1, 0), True),
      ((1, 1), False),
      ((2, 0), True),
      ((2, 1), True)
    ]

printTile :: Tile -> String
printTile =
  unlines
    . drawGraph (\case Nothing -> ' '; Just True -> '#'; Just False -> '.')
    . Map.fromList
    . assocs

day20 :: IO ()
day20 = do
  input <-
    map
      ( ( (,)
            <$> ( read @Int
                    . filter isNumber
                    . head
                )
            <*> ( \x ->
                    let m =
                          drawMap (\case '#' -> Just True; '.' -> Just False)
                            . tail
                            $ x
                        (a, b) = (,) <$> minimum <*> maximum $ Map.keys m
                     in array (a, b) $ Map.assocs m :: UArray Index Bool
                )
        )
          . lines
      )
      . splitOn "\n\n"
      -- <$> readFile "input/test20.txt"
      <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  let x = snd $ input !! 4
      y = flipTile $ rotateTile x
      input' = map snd input
  putStrLn
    . ("day20a: " ++)
    . show
    . product
    . map fst
    $ filter ((== Corner) . (`isTypeIn` input') . snd) input
  let bigPicMap = picMap input'
      bigPic = stitch bigPicMap
      dragonN = fmap length
        . find (not . null)
        . map (findDragon dragon . snd)
        $ tileVar bigPic
      dragonL = length $ filter id $ elems dragon
      bigPicL = length $ filter id $ elems bigPic
  putStrLn
    . ("day20b: " ++)
    . show
    $ subtract <$> fmap (* dragonL) dragonN <*> pure bigPicL
