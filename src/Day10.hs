module Day10 where
import Data.List (sort, group)
import Data.List.Split (divvy)


day10 :: IO ()
day10 = do
  input <- sort . map (read @Int) . lines <$> readFile "input/input10.txt"
  let day10a = map length $ group $ sort $ map (subtract <$> head <*> (!! 1)) $ divvy 2 1 input
  print day10a
