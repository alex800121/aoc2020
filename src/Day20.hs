{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day20 where

import Control.Applicative (liftA, liftA2)
import Control.Monad (guard)
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Bifunctor (bimap)
import Data.Char (isNumber)
import Data.Function ((&))
import Data.List (delete, foldl', nub, (\\))
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import MyLib (Direction (..), drawGraph, drawMap)

type Tile = UArray Index Bool

data TileType = Corner | Side | Center deriving (Show, Eq, Ord)

type TileVar = (Op' Int, Tile)

type Index = (Int, Int)

data Op = Flip | Rotate deriving (Show, Eq, Ord)

data Op' a = Op {flipN :: a, rotateN :: a} deriving (Show, Eq, Ord, Functor)

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

sharedSides :: Tile -> Tile -> Maybe (Tile, Tile, Index)
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
      return (ref, reverseOp rOp t, reverseIndex rOp rIndex)

reverseIndex :: Op' Int -> Index -> Index
reverseIndex (Op a b) i =
  foldl'
    (\acc x -> x acc)
    i
    (replicate (a `mod` 2) flipIndex <> replicate (negate b `mod` 4) rotateIndex )
  where
    flipIndex (x, y) = (-x, y)
    rotateIndex (x, y) = (-y, x)

op, reverseOp :: Op' Int -> Tile -> Tile
op (Op a b) t =
  foldl'
    (\acc x -> x acc)
    t
    (replicate (b `mod` 4) rotateTile <> replicate (a `mod` 2) flipTile)
reverseOp (Op a b) t =
  foldl'
    (\acc x -> x acc)
    t
    (replicate (a `mod` 2) flipTile <> replicate (negate b `mod` 4) rotateTile )

sides :: Tile -> [(Direction, [Bool])]
sides t =
  map
    (fmap (map (t !)))
    [ (North, [(x, minY) | x <- [minX .. maxX]]),
      (West, [(maxX, y) | y <- [minY .. maxY]]),
      (South, [(x, maxY) | x <- reverse [minX .. maxX]]),
      (East, [(minX, y) | y <- reverse [minY .. maxY]])
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
        ( \(x, y) -> (negate x + fst (snd i) - fst (fst i), y)
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
      <$> readFile "input/input20.txt"
  let x = snd $ input !! 4
      y = flipTile $ rotateTile x
      input' = map snd input
  -- putStrLn
  --   . ("day20a: " ++)
  --   . show
  --   . product
  --   . map fst
  --   $ filter ((== Corner) . (`isTypeIn` input') . snd) input
  putStrLn $ printTile x
  mapM_ (putStrLn . (\(_, y, z) -> printTile y ++ show z ++ "\n")) $ mapMaybe (sharedSides x) input'
