module Day25 where

import Data.List (elemIndex, findIndex)

pubKey1 = 1526110
pubKey2 = 20175123
-- pubKey1 = 5764801
-- pubKey2 = 17807724

subject = 7

divisor = 20201227

step :: Integer -> Integer -> Integer
step s n = (n * s) `mod` divisor

loopSize1 = elemIndex pubKey1 $ iterate (step 7) 1

loopSize2 = elemIndex pubKey2 $ iterate (step 7) 1

encrypt1 = ans <$> loopSize1 <*> pure pubKey2 <*> pure 1
encrypt2 = ans <$> loopSize2 <*> pure pubKey1 <*> pure 1


ans :: Int -> Integer -> Integer -> Integer
ans n s
  | n <= 0 = id
  | otherwise = ans (n - 1) s . step s

day25 :: IO ()
day25 = do
  putStrLn $ ("day25: " ++) $ show encrypt1
  putStrLn "Merry Christmas!!!"
