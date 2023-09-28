module Day21 where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)

type Allergen = String

type Ingredient = String

data Food = Food
  { _ingredient :: Set Ingredient,
    _allergen :: Set Allergen
  }
  deriving (Show, Eq)

instance Ord Food where
  compare (Food ia aa) (Food ib ab) =
    compare (Set.size aa) (Set.size ab)
      <> compare (Set.size ia) (Set.size ib)
      <> compare aa ab
      <> compare ia ib

readFood :: String -> Food
readFood s = Food i a
  where
    x : y : _ = splitOn " (contains " $ init s
    i = Set.fromList $ words x
    a = Set.fromList $ splitOn ", " y

day21 :: IO ()
day21 = do
  input <- Set.fromList . map readFood . lines <$> readFile "input/input21.txt"
  let allergens = Set.size $ _allergen $ Set.foldl' (\(Food a b) (Food c d) -> Food (Set.union a b) (Set.union c d)) (Food Set.empty Set.empty) input
  print allergens
