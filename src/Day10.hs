module Day10 where

import Data.List (group, sort, unfoldr, find)
import Data.List.Split (divvy)

day10 :: IO ()
day10 = do
  -- input' <- sort . map (read @Int) . lines <$> readFile "input/test10.txt"
  input' <- sort . map (read @Int) . lines <$> readFile "input/input10.txt"
  let input = 0 : input' ++ [last input' + 3]
      day10a = product $ map length $ group $ sort $ map (subtract <$> head <*> (!! 1)) $ divvy 2 1 input
      target = last input
      day10b =
        find ((== target) . fst) $
          unfoldr
            ( \(a, b, c) ->
                let i = fst c + 1
                    d = if i `elem` input then sum $ map snd [a, b, c] else 0
                 in Just (c, (b, c, (i, d)))
            )
            ((-2, 0), (-1, 0), (0, 1))
  putStrLn $ ("day10a: " ++) $ show day10a
  putStrLn $ ("day10b: " ++) $ show day10b
