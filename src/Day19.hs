module Day19 where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isNumber)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (find, foldl')
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    choice,
    eof,
    get,
    many,
    many1,
    readP_to_S,
    satisfy,
    string,
  )

type Test = ReadP ()

type InitMap = IntMap (Either [[Int]] Char)

-- type TestMap = IntMap Test

initMap :: ReadP InitMap
initMap = do
  i <- read @Int <$> many (satisfy isNumber) <* string ": "
  n <- (Right <$> (char '"' >> get <* char '"')) <|> (Left <$> f id)
  return $ IM.singleton i n
  where
    f g =
      (eof >> return [g []])
        <|> (char '|' >> char ' ' >> (g [] :) <$> f id)
        <|> do
          a <- read @Int <$> many1 (satisfy isNumber) <* (void (char ' ') <|> eof)
          f (g . (a :))

-- buildTest' m i = buildTest m i >> eof

buildTest :: InitMap -> Int -> Test
buildTest m i = case m IM.!? i of
  Nothing -> error "No rule"
  Just (Right c) -> void $ char c
  Just (Left l) ->
    let xs = map (foldl' (\acc x -> acc >> buildTest m x) (pure ())) l
     in void $ choice xs

day19 :: IO ()
day19 = do
  a : b : _ <- map lines . splitOn "\n\n" <$> readFile "input/input19.txt"
  -- a : b : _ <- map lines . splitOn "\n\n" <$> readFile "input/test19.txt"
  let test = IM.unions $ map (fst . head . readP_to_S initMap) a
      test0 = buildTest test 0
      test' =
        IM.insert 11 (Left [[42, 31], [42, 11, 31]])
          . IM.insert 8 (Left [[42], [42, 8]])
          $ test
      test0' = buildTest test' 0
  putStrLn
    . ("day19a: " ++)
    . show
    . length
    $ filter (any (null . snd) . readP_to_S test0) b
  putStrLn
    . ("day19b: " ++)
    . show
    . length
    $ filter (any (null . snd) . readP_to_S test0') b
