module Day21 where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)
import Data.List (intercalate)

type Allergen = String

type Ingredient = String

type AllergenMap = Map Allergen (Set Ingredient)

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

allergenMap :: [Food] -> AllergenMap
allergenMap = Map.unionsWith Set.intersection . map (\(Food x y) -> Map.fromSet (const x) y)

reduceAllergen :: AllergenMap -> AllergenMap
reduceAllergen m
  | Map.null single = multiple
  | otherwise = Map.union single $ reduceAllergen multiple'
  where
    (single, multiple) = Map.partition ((== 1) . Set.size) m
    multiple' = Map.map (Set.\\ (Set.unions $ Map.elems single)) multiple

day21 :: IO ()
day21 = do
  input <- map readFood . lines <$> readFile "input/input21.txt"
  let allergens = foldr (\x acc -> Set.union acc (_allergen x))  Set.empty input
      x = reduceAllergen $ allergenMap input
      withAllergen = Set.unions $ Map.elems x
  putStrLn
    . ("day21a: " ++)
    . show
    . sum
    . map (Set.size . (Set.\\ withAllergen) . _ingredient)
    $ input
  putStrLn
    . ("day21b: " ++)
    . intercalate ","
    . map (Set.findMin . snd)
    . Map.assocs
    $ x
