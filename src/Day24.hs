{-# LANGUAGE TupleSections #-}

module Day24 where

import Data.List (foldl')
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Set (Set)
import qualified Data.Set as Set

type Index = (Int, Int)

type Floor = Set Index

data Direction
  = E
  | SE
  | SW
  | W
  | NW
  | NE
  deriving (Show, Eq, Ord, Bounded, Enum)

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

next :: Floor -> Floor
next xs = blacks `Set.union` whites
  where
    xss = MS.unions $ map (MS.fromSet . (\d -> Set.map (`readDirection` d) xs)) [minBound .. maxBound]
    blacks = Set.filter (\i -> MS.occur i xss `elem` [1, 2]) xs
    whites = MS.toSet $ MS.filter (\i -> Set.notMember i xs && MS.occur i xss == 2) xss

day24 :: IO ()
day24 = do
  -- input <- lines <$> readFile "input/test24.txt"
  input <- lines <$> readFile "input/input24.txt"
  let initFloor =
        Set.fromList
          . map fst
          . filter (odd . snd)
          . MS.toOccurList
          . MS.fromList
          . map (foldl' readDirection (0, 0) . parseDirection)
          $ input
  putStrLn
    . ("day24a: " ++)
    . show
    . length
    $ initFloor
  putStrLn
    . ("day24b: " ++)
    . show
    . length
    $ iterate next initFloor !! 100
