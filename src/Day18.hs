{-# LANGUAGE TupleSections #-}

module Day18 where


import Paths_AOC2020
import MyLib (Parser, signedInteger)

import Text.Megaparsec (anySingle, many, optional, parseMaybe, (<|>))

import Text.Megaparsec.Char (char, space)

data Op = Add | Mul deriving (Show, Eq, Ord)

data Tree a = Leaf a | Branch Op (Tree a) (Tree a) deriving (Show, Eq, Ord)

type Expr = Tree Int

buildLTree :: Expr -> [(Op, Expr)] -> Expr
buildLTree x [] = x
buildLTree x ((op, y) : ys) = buildLTree (Branch op x y) ys

term, term1, paren, paren1, expr, expr1, expr2 :: Parser Expr
term = (Leaf <$> signedInteger <|> paren) <* optional space
paren = char '(' >> expr <* char ')'
expr = do
  x <- term
  xs <-
    many
      ( (char '+' >> optional space >> (Add,) <$> term)
          <|> (char '*' >> optional space >> (Mul,) <$> term)
      )
  return $ buildLTree x xs
term1 = (Leaf <$> signedInteger <|> paren1) <* optional space
paren1 = char '(' >> expr2 <* char ')'
expr1 = do
  x <- term1
  xs <- many (char '+' >> optional space >> (Add,) <$> term1)
  return $ buildLTree x xs
expr2 = do
  x <- expr1
  xs <- many (char '*' >> optional space >> (Mul,) <$> expr1)
  return $ buildLTree x xs

calcTree :: (Num a) => Tree a -> a
calcTree (Leaf a) = a
calcTree (Branch o a b) = op o (calcTree a) (calcTree b)

op :: (Num a) => Op -> a -> a -> a
op Add = (+)
op Mul = (*)

day18 :: IO ()
day18 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input18.txt"))
  let input1 = traverse (parseMaybe expr) input
      input2 = traverse (parseMaybe expr2) input
  putStrLn
    . ("day18a: " ++)
    . show
    $ fmap (sum . fmap calcTree) input1
  putStrLn
    . ("day18b: " ++)
    . show
    $ fmap (sum . fmap calcTree) input2
