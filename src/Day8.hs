module Day8 where


import Paths_AOC2020
import MyLib (firstRepeatBy)

import Data.Array.IArray

import Control.Monad (foldM)

import Data.List (unfoldr, find)

import Data.Function (on)

import Data.Maybe (isNothing)

data Instruction a
  = Acc a
  | Jmp a
  | Nop a
  deriving (Show, Eq, Ord)

data Machine = M {_acc :: Int, _ins :: Array Int (Instruction Int), _n :: Int}
  deriving (Show, Eq, Ord)

insParser :: String -> Instruction Int
insParser s = case words s of
  "acc" : x : _ -> Acc $ f x
  "jmp" : x : _ -> Jmp $ f x
  "nop" : x : _ -> Nop $ f x
  where
    f = read @Int . filter (/= '+')

run :: Machine -> Maybe Machine
run (M acc ins n)
  | n < lowerB || n > upperB = Nothing
  | otherwise = Just $ case ins ! n of
    Acc x -> M (acc + x) ins (n + 1)
    Jmp x -> M acc ins (n + x)
    Nop x -> M acc ins (n + 1)
  where
    (lowerB, upperB) = bounds ins

changeOne :: Array Int (Instruction Int) -> [Array Int (Instruction Int)]
changeOne a = a'
  where
    b = bounds a
    l = assocs a
    f = do
      x <- l
      case x of
        (i, Nop y) -> return (i, Jmp y)
        (i, Jmp y) -> return (i, Nop y)
        y -> return y
    a' = [a // [x] | x <- f]

day8 :: IO ()
day8 = do
  input <- zip [0..] . map insParser . lines <$> (getDataDir >>= readFile . (++ "/input/input8.txt"))
  let initMachine = M 0 (array (0, length input - 1) input) 0
      l = unfoldr (fmap (\x -> (x, x)) . run) initMachine
      x = _acc . (l !!) . subtract 1 . fst <$> firstRepeatBy ((==) `on` _n) l
      initMachines = map (\x -> initMachine {_ins = x}) $ changeOne $ _ins initMachine
  putStrLn $ ("day8a: " ++) $ show x
  putStrLn $ ("day8b: " ++) $ show $ fmap (_acc . last) $ find (isNothing . firstRepeatBy ((==) `on` _n)) $ map (unfoldr (fmap (\x -> (x, x)) . run)) initMachines
  
