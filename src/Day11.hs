{-# LANGUAGE LambdaCase #-}
module Day11 where


import Paths_AOC2020
import Data.Map (Map)

import qualified Data.Map as Map

import MyLib (drawMap, stablized, drawGraph)

import Data.Maybe (mapMaybe, fromMaybe)

import Data.Bifunctor (Bifunctor(bimap))

import Control.Monad (join)

type Index = (Int, Int)
type SeatMap = Map Index (Maybe Bool)

step :: SeatMap -> SeatMap
step s = Map.mapWithKey (fmap . f) s
  where
    f k True = 4 > length (filter id (mapMaybe (join . (s Map.!?) . bimap (+ fst k) (+ snd k)) surrounding))
    f k False = all not (mapMaybe (join . (s Map.!?) . bimap (+ fst k) (+ snd k)) surrounding)

step' :: SeatMap -> SeatMap
step' s = Map.mapWithKey f s
  where
    f _ Nothing = Nothing
    f k (Just True) = Just $ 5 > length (filter (seeOccupied s k) surrounding)
    f k (Just False) = Just $ not (any (seeOccupied s k) surrounding)

seeOccupied :: SeatMap -> Index -> Index -> Bool
seeOccupied s i (x, y) = go (bimap (+ x) (+ y) i)
  where
    go n = case s Map.!? n of
      Nothing -> False
      Just Nothing -> go (bimap (+ x) (+ y) n)
      Just (Just True) -> True
      Just (Just False) -> False

surrounding :: [Index]
surrounding = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

day11 :: IO ()
day11 = do
  input <- drawMap (\case ; 'L' -> Just (Just False) ; '#' -> Just (Just True) ; _ -> (Just Nothing)) . lines <$> (getDataDir >>= readFile . (++ "/input/input11.txt"))
  -- input <- drawMap (\case ; 'L' -> Just (Just False) ; '#' -> Just (Just True) ; _ -> (Just Nothing)) . lines <$> readFile "input/test11.txt"
  putStrLn $ ("day11a: " ++) $ show $ fmap (Map.size . Map.filter (fromMaybe False) . snd) $ stablized $ iterate step input
  putStrLn $ ("day11b: " ++) $ show $ fmap (Map.size . Map.filter (fromMaybe False) . snd) $ stablized $ iterate step' input
