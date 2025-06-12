{-# LANGUAGE LambdaCase #-}

module Day14 where

import Control.Monad (foldM)
import Data.Bits (Bits (clearBit, setBit))
import Data.List (foldl', unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple (swap)
import MyLib (Parser)
import Paths_AOC2020
import Text.Megaparsec (anySingleBut, many, parseMaybe, parseTest, takeRest, (<|>))
import Text.Megaparsec.Char (string)

type Mask = [(Int, Maybe Bool)]

type Bit = [Bool]

data Instruction = Mask {_mask :: Mask} | Mem {_addr :: Int, _val :: Word}
  deriving (Show, Eq, Ord)

type Mem = Map Int Word

type Machine = (Mask, Mem)

bitToDec :: (Integral a) => Bit -> a
bitToDec = foldl' (\acc x -> acc * 2 + if x then 1 else 0) 0

decToBit :: Word -> Bit
decToBit = go []
  where
    go l n = case n `divMod` 2 of
      (0, 0) -> l
      (q, r) -> go ((r == 1) : l) q

mem :: Parser Instruction
mem = do
  string "mem["
  n <- read <$> many (anySingleBut ']')
  string "] = "
  Mem n . read <$> takeRest

mask :: Parser Instruction
mask = do
  string "mask = "
  Mask
    . zip [0 ..]
    . reverse
    . map
      ( \case
          '0' -> Just False
          '1' -> Just True
          _ -> Nothing
      )
    <$> takeRest

parser :: Parser Instruction
parser = mask <|> mem

maskWith :: Word -> Mask -> Word
maskWith =
  foldl'
    ( \w (n, x) ->
        case x of
          Nothing -> w
          Just True -> w `setBit` n
          Just False -> w `clearBit` n
    )

decodeAddr :: Int -> Mask -> [Int]
decodeAddr =
  foldM
    ( \acc (i, x) -> case x of
        Nothing -> [acc `setBit` i, acc `clearBit` i]
        Just True -> [acc `setBit` i]
        Just False -> [acc]
    )

readIns :: Machine -> Instruction -> Machine
readIns (m, n) (Mem i j) = (m, Map.insert i (j `maskWith` m) n)
readIns (m, n) (Mask i) = (i, n)

readIns2 :: Machine -> Instruction -> Machine
readIns2 (m, n) (Mem i j) = (m, foldl' (\acc x -> Map.insert x j acc) n $ decodeAddr i m)
readIns2 (m, n) (Mask i) = (i, n)

day14 :: IO ()
day14 = do
  -- input <- mapMaybe (parseMaybe parser) . lines <$> readFile "input/test14.txt"
  input <- mapMaybe (parseMaybe parser) . lines <$> (getDataDir >>= readFile . (++ "/input/input14.txt"))
  putStrLn
    . ("day14a: " ++)
    . show
    . sum
    . Map.elems
    . snd
    $ foldl' readIns ([], Map.empty) input
  putStrLn
    . ("day14b: " ++)
    . show
    . sum
    . Map.elems
    . snd
    $ foldl' readIns2 ([], Map.empty) input
