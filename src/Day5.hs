module Day5 where
import Data.List (foldl', sort, (\\))

day5 :: IO ()
day5 = do
  input <- lines <$> readFile "input/input5.txt"
  let seatIDs = sort $ map (foldl' (\acc x -> acc * 2 + if x `elem` "FL" then 0 else 1) 0) input
      b = maximum seatIDs
      a = minimum seatIDs
  putStrLn $ ("day5a: " ++) $ show $ maximum seatIDs
  putStrLn $ ("day5a: " ++) $ show $ [a .. b] \\ seatIDs
