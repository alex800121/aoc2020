module Day24 where

import Data.List (foldl')
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ix (range)
import Data.Bifunctor (bimap)

type Index = (Int, Int)

data Direction
  = E
  | SE
  | SW
  | W
  | NW
  | NE
  deriving (Show, Eq, Ord, Bounded, Enum)

type State = ((Index, Index), Set Index)

nextState :: State -> State
nextState (((a, b), (c, d)), s) = (r, Set.fromList $ filter f l)
  where
    r = ((a - 1, b - 1), (c + 1, d + 1))
    l = range r
    f i
      | i `Set.member` s = j /= 0 && j <= 2
      | otherwise = j == 2
      where
        j = length $ filter (`Set.member` s) $ map (bimap (+ fst i) (+ snd i)) adjacents

adjacents :: [Index]
adjacents = map (readDirection (0, 0)) [minBound .. maxBound]

parseDirection :: String -> [Direction]
parseDirection "" = []
parseDirection ('e' : xs) = E : parseDirection xs
parseDirection ('s' : 'e' : xs) = SE : parseDirection xs
parseDirection ('s' : 'w' : xs) = SW : parseDirection xs
parseDirection ('w' : xs) = W : parseDirection xs
parseDirection ('n' : 'w' : xs) = NW : parseDirection xs
parseDirection ('n' : 'e' : xs) = NE : parseDirection xs

{-

##
###
 ##

-}

readDirection :: Index -> Direction -> Index
readDirection (x, y) d = case d of
  E -> (x + 1, y)
  SE -> (x + 1, y + 1)
  SW -> (x, y + 1)
  W -> (x - 1, y)
  NW -> (x - 1, y - 1)
  NE -> (x, y - 1)

day24 :: IO ()
day24 = do
  -- input <- lines <$> readFile "input/test24.txt"
  input <- lines <$> readFile "input/input24.txt"
  let initTiles =
        MS.foldOccur (\a o -> if odd o then Set.insert a else id) Set.empty
          . MS.fromList
          . map (foldl' readDirection (0, 0) . parseDirection)
          $ input
      ix =
        Set.fold
          (\(x, y) ((a, b), (c, d)) -> ((min x a, min y b), (max x c, max y d)))
          ((0, 0), (0, 0))
          initTiles
      initState = (ix, initTiles)
  putStrLn
    . ("day24a: " ++)
    . show
    . Set.size
    $ initTiles
  putStrLn
    . ("day24b: " ++)
    . show
    . Set.size
    . snd
    . (!! 100)
    . iterate nextState
    $ initState
