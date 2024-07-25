module Day16 where


import Paths_AOC2020
import Control.Monad (void)

import Data.Function (on)

import Data.List (delete, sortBy, transpose, uncons, isPrefixOf)

import Data.List.Split (splitOn)

import Debug.Trace (traceShow)

type Range = (Int, Int)

type FieldRules = [(String, [Range])]

type Ticket = [Int]

inputParser :: String -> (FieldRules, Ticket, [Ticket])
inputParser s = (f, t, ts)
  where
    a : b : c : _ = splitOn "\n\n" s
    f =
      map
        ( ( \(x : y : _) ->
              let y' =
                    map
                      ( (\(n : m : _) -> (n, m))
                          . map read
                          . splitOn "-"
                      )
                      $ splitOn " or " y
               in (x, y')
          )
            . splitOn ": "
        )
        $ lines a
    t =
      map read
        . splitOn ","
        . (!! 1)
        $ lines b
    ts =
      map (map read . splitOn ",")
        . tail
        $ lines c

validField :: FieldRules -> Int -> Bool
validField fr i = any (withinRange i) $ concatMap snd fr

withinRange :: Int -> Range -> Bool
withinRange i (a, b) = a <= i && i <= b

calcValidFields :: [[Int]] -> FieldRules -> [[(String, Int)]]
calcValidFields _ [] = [[]]
calcValidFields i ((name, ranges) : fr') = do
  x <- filter (all (\y -> any (withinRange y) ranges)) i
  let xs = delete x i
  ((name, head x) :) <$> calcValidFields xs fr'

day16 :: IO ()
day16 = do
  -- (fr, t, ts) <- inputParser <$> readFile "input/test16.txt"
  (fr, t, ts) <- inputParser <$> (getDataDir >>= readFile . (++ "/input/input16.txt"))
  let ts' =
        sortBy
          ( compare
              `on` ( \x ->
                       length
                         ( filter
                             (\(_, y) -> all (\z -> any (withinRange z) y) x)
                             fr
                         )
                   )
          )
          . transpose
          $ t : filter (all (validField fr)) ts
  putStrLn
    . ("day16a: " ++)
    . show
    . sum
    . filter (not . validField fr)
    $ concat ts
  putStrLn
    . ("day16a: " ++)
    . show
    . product
    . map snd
    . filter (isPrefixOf "departure" . fst)
    . head
    $ calcValidFields ts' fr
