module Day8 where

import Data.Array.IArray
import Control.Monad (foldM)
import Data.List (unfoldr)

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

day8 :: IO ()
day8 = do
  input <- zip [0..] . map insParser . lines <$> readFile "input/input8.txt"
  let initMachine = M 0 (array (0, length input - 1) input) 0
  print $ take 20 $ unfoldr (fmap (\x -> (x, x)) . run) initMachine
