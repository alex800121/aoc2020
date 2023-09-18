module Day7 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import MyLib (Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space, string)

type Bag = Map String (Map String Int)

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
  content <- contentParser
  return $ Map.singleton name (Map.fromList content)

containBag :: Bag -> String -> Set String -> Set String -> Set String
containBag b s next acc
  | Set.null next = Set.delete s acc
  | otherwise = containBag b s next' acc'
  where
    acc' = Set.union acc next
    next' = (Set.\\ acc) $ Map.keysSet $ Map.filter (any (`Set.member` next) . Map.keys) b

countBags :: Bag -> String -> Int
countBags b s = maybe 0 (sum . Map.mapWithKey (\k a -> a + a * countBags b k)) (b Map.!? s)

day7 :: IO ()
day7 = do
  -- bags <- Map.unionsWith Map.union . mapMaybe (parseMaybe bagParser) . lines <$> readFile "input/test7.txt"
  bags <- Map.unionsWith Map.union . mapMaybe (parseMaybe bagParser) . lines <$> readFile "input/input7.txt"
  putStrLn $ ("day7a: " ++) $ show $ length $ containBag bags "shiny gold" (Set.singleton "shiny gold") Set.empty
  putStrLn $ ("day7b: " ++) $ show $ countBags bags "shiny gold"
