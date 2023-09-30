module Day23 where

import Data.Char (digitToInt)
import Data.Sequence
import qualified Data.Sequence as Seq

input :: Seq Int
-- input = Seq.fromList $ map digitToInt "389125467"
input = Seq.fromList $ map digitToInt "962713854"

step :: Seq Int -> Seq Int
step (x :<| xs) = (xs10 >< xs0 >< xs11) |> x
  where
    (xs0, xs1) = Seq.splitAt 3 xs
    f n
      | n < 1 = f 9
      | n `elem` xs0 = f (n - 1)
      | otherwise = n
    target = f (x - 1)
    (xs11, xs10) = spanr (/= target) xs1

day23 :: IO ()
day23 = do
  let ans = iterate step input
      input' = input >< Seq.fromList [10..1000000]
  putStrLn
    . ("day23a: " ++)
    . show
    $ ans !! 100
  -- putStrLn
  --   . ("day23b: " ++)
  --   . show
  --   . dropWhileL (/= 1)
  --   . (!! 10000000)
  --   $ iterate step input'
  mapM_ print $ Prelude.take 100 ans
