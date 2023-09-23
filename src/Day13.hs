module Day13 where

import Control.Monad (foldM)
import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import MyLib (extEuc, mapFirst)

type Bus = (Integer, Integer)

mergeBus :: Bus -> Bus -> Maybe Bus
mergeBus (t1, s1) (t2, s2)
  | r /= 0 = Nothing
  | otherwise = Just ((((a * q * s1) `mod` d) + d) `mod` d + t1, d)
  where
    (a, b, c) = extEuc s1 s2
    (q, r) = (t2 - t1) `divMod` c
    d = lcm s1 s2

day13 :: IO ()
day13 = do
  a : b : _ <- lines <$> readFile "input/input13.txt"
  let departureTime = read @Integer a
      bus =
        map (fmap (read @Integer))
          . filter ((/= "x") . snd)
          . zip [0 ..]
          $ splitOn "," b
      day13b =
        fmap fst
          . foldM mergeBus (0, 1)
          $ map (mapFirst negate) bus
  putStrLn
    . ("day13a: " ++)
    . show
    . uncurry (*)
    . minimumBy (compare `on` snd)
    $ map (\(_, b) -> (b, ((b - (departureTime `mod` b)) `mod` b))) bus
  putStrLn
    . ("day13b: " ++)
    . show
    $ day13b
