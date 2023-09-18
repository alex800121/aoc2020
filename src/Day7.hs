module Day7 where

import Data.Map (Map)
import Control.Monad.Trans.State.Strict (State, evalState, get, modify)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromMaybe)
import MyLib (Parser, signedInteger)
import Text.Megaparsec (anySingle, (<|>), parseMaybe)
import Text.Megaparsec.Char (char, space, string)
import Control.Monad (foldM)

type Bag = Map String [(String, Int)]

nameParser :: Parser String
nameParser = (string " bags" >> return "") <|> (string " bag" >> return "") <|> ((:) <$> anySingle <*> nameParser)

contentParser :: Parser [(String, Int)]
contentParser =
  (string "no other bags." >> return []) <|> do
    a <- signedInteger <* space
    b <- nameParser
    ((b, a) :) <$> ((char '.' >> return []) <|> (string ", " >> contentParser))

bagParser :: Parser Bag
bagParser = do
  name <- nameParser <* string " contain "
  Map.singleton name <$> contentParser

containBag :: Bag -> String -> Set String -> Set String -> Set String
containBag b s next acc
  | Set.null next = Set.delete s acc
  | otherwise = containBag b s next' acc'
  where
    acc' = Set.union acc next
    next' = (Set.\\ acc) $ Map.keysSet $ Map.filter (any ((`Set.member` next) . fst)) b

countBags :: Bag -> String -> State (Map String Int) Int
countBags b s = do
  cache <- get
  case cache Map.!? s of
    Nothing -> do
      ans <- foldM (\acc (k, a) -> (acc + a +) . (a *) <$> countBags b k) 0 $ fromMaybe [] (b Map.!? s)
      -- ans <- maybe 0 (sum <$> traverse . Map.mapWithKeyM (\k a -> a + a * countBags b k)) (b Map.!? s)
      modify (Map.insert s ans)
      return ans
    Just x -> return x

day7 :: IO ()
day7 = do
  -- bags <- Map.unionsWith Map.union . mapMaybe (parseMaybe bagParser) . lines <$> readFile "input/test7.txt"
  bags <- Map.unionsWith (<>) . mapMaybe (parseMaybe bagParser) . lines <$> readFile "input/input7.txt"
  putStrLn $ ("day7a: " ++) $ show $ length $ containBag bags "shiny gold" (Set.singleton "shiny gold") Set.empty
  putStrLn $ ("day7b: " ++) $ show $ (`evalState` Map.empty) $ countBags bags "shiny gold"
