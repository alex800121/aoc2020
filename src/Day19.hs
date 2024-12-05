{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Day19 where

import Control.Applicative (Alternative (..), many, some, (<|>))
import Control.Monad (foldM_, void)
import Control.Monad.Hefty
import Control.Monad.Hefty.NonDet
import Control.Monad.Hefty.State
import Data.Char (isNumber)
import Data.Foldable (traverse_)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (find, foldl', foldl1')
import Data.List.Split (splitOn)
import HeftiaParser
import Paths_AOC2020
import Data.Maybe (mapMaybe, fromJust)

type InitMap = IntMap (Either [[Int]] Char)

initMap :: forall ef eh. (ChooseH <<| eh, Empty <| ef, State String <| ef) => Eff eh ef InitMap
initMap = do
  i <- read @Int <$> many (satisfy isNumber) <* string ": "
  n <- (Right <$> (char '"' >> anySingle <* char '"')) <|> (Left <$> f id)
  return $ IM.singleton i n
  where
    f g =
      (eof >> return [g []])
        <|> (char '|' >> char ' ' >> (g [] :) <$> f id)
        <|> do
          a <- read @Int <$> some (satisfy isNumber) <* (void (char ' ') <|> eof)
          f (g . (a :))

buildTest :: forall ef eh. (Choose <| ef, Empty <| ef, State String <| ef) => InitMap -> Int -> Eff eh ef ()
buildTest m i = case m IM.!? i of
  Nothing -> error "No rule"
  Just (Right c) -> void $ char c
  Just (Left l) -> do
    xs <- choice l
    traverse_ (buildTest m) xs

day19 :: IO ()
day19 = do
  a : b : _ <- map lines . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  let test :: InitMap =
        IM.unions $
          mapMaybe
            ( \x ->
                runPure $ runNonDetMaybe $ evalState x $ runChooseH initMap
            )
            a
      test0 = buildTest test 0
      test' :: InitMap =
        IM.insert 11 (Left [[42, 31], [42, 11, 31]])
          . IM.insert 8 (Left [[42], [42, 8]])
          $ test
      test0' = buildTest test' 0
  putStrLn
    . ("day19a: " ++)
    . show
    . length
    $ mapMaybe (\x -> runPure . runNonDetMaybe . runState x $ runChooseH (test0 >> eof)) b
  putStrLn
    . ("day19b: " ++)
    . show
    . length
    $ mapMaybe (\x -> runPure . runNonDetMaybe . runState x $ runChooseH (test0' >> eof)) b
