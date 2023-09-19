module Day12 where

import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (foldl')
import MyLib (Direction (..))

data Instruction = Move Direction Int | Forward Int | Turn Side Int deriving (Show, Eq, Ord)

data Side = R | L deriving (Show, Eq, Ord)

instance Enum Side where
  toEnum x = case x `mod` 2 of
    0 -> R
    1 -> L
  fromEnum R = 0
  fromEnum L = 1

type Index = (Int, Int)

data Ferry = Ferry {_index :: Index, _direction :: Direction} deriving (Show, Eq, Ord)

data Ferry2 = Ferry2 {_index2 :: Index, _direction2 :: Index} deriving (Show, Eq, Ord)

initFerry = Ferry (0, 0) East

initFerry2 = Ferry2 (0, 0) (10, 1)

inputParser :: String -> Instruction
inputParser (x : xs) =
  ( case x of
      'L' -> Turn L . (`div` 90)
      'R' -> Turn R . (`div` 90)
      'F' -> Forward
      'S' -> Move South
      'N' -> Move North
      'W' -> Move West
      'E' -> Move East
  )
    (read xs)

forward2 :: Int -> Ferry2 -> Ferry2
forward2 n ferry@(Ferry2 i d) = ferry {_index2 = bimap (+ (n * fst d)) (+ (n * snd d)) i}

turn2 :: Side -> Int -> Ferry2 -> Ferry2
turn2 s i ferry = ferry {_direction2 = iterate f (_direction2 ferry) !! i}
  where
    f = case s of
      R -> \(x, y) -> (y, -x)
      L -> \(x, y) -> (-y, x)

move2 :: Direction -> Int -> Ferry2 -> Ferry2
move2 d i ferry =
  let d' = case d of
        North -> (0, i)
        South -> (0, -i)
        West -> (-i, 0)
        East -> (i, 0)
   in ferry {_direction2 = bimap (+ fst d') (+ snd d') $ _direction2 ferry}

readIns2 :: Ferry2 -> Instruction -> Ferry2
readIns2 ferry ins = case ins of
  Move d i -> move2 d i ferry
  Forward i -> forward2 i ferry
  Turn t i -> turn2 t i ferry

move :: Direction -> Int -> Ferry -> Ferry
move d i ferry =
  let d' = case d of
        North -> (0, i)
        South -> (0, -i)
        West -> (-i, 0)
        East -> (i, 0)
   in ferry {_index = bimap (+ fst d') (+ snd d') $ _index ferry}

turn :: Side -> Int -> Ferry -> Ferry
turn s i ferry =
  ferry
    { _direction = case s of
        R -> iterate succ (_direction ferry) !! i
        L -> iterate pred (_direction ferry) !! i
    }

readIns :: Ferry -> Instruction -> Ferry
readIns ferry ins = case ins of
  Move d i -> move d i ferry
  Forward i -> move (_direction ferry) i ferry
  Turn t i -> turn t i ferry

day12 :: IO ()
day12 = do
  -- input <- map inputParser . lines <$> readFile "input/test12.txt"
  input <- map inputParser . lines <$> readFile "input/input12.txt"
  putStrLn $ ("day12a: " ++) $ show $ uncurry ((+) `on` abs) $ _index $ foldl' readIns initFerry input
  putStrLn $ ("day12b: " ++) $ show $ uncurry ((+) `on` abs) $ _index2 $ foldl' readIns2 initFerry2 input
