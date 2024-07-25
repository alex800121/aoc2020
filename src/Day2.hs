module Day2 where


import Paths_AOC2020
import Data.Char (isNumber)

import Data.Maybe (mapMaybe)

import MyLib (Parser)

import Text.Megaparsec

import Text.Megaparsec.Char (char, space, string)

data PW = PW {range :: (Int, Int), character :: Char, pw :: String} deriving (Show, Eq)

inputParser :: Parser PW
inputParser = do
  a <- read @Int <$> many (satisfy isNumber) <* char '-'
  b <- read @Int <$> many (satisfy isNumber) <* space
  c <- anySingle <* string ": "
  PW (a, b) c <$> takeRest

isValid :: PW -> Bool
isValid (PW (x, y) c pw) = let l = length $ filter (== c) pw in l >= x && l <= y

isValid2 :: PW -> Bool
isValid2 (PW (x, y) c pw) =
  let x' = pw !! (x - 1)
      y' = pw !! (y - 1)
   in x' /= y' && (x' == c || y' == c)

day2 :: IO ()
day2 = do
  input <- mapMaybe (parseMaybe inputParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input2.txt"))
  putStrLn $ ("day2a: " ++) $ show $ length $ filter isValid input
  putStrLn $ ("day2b: " ++) $ show $ length $ filter isValid2 input
