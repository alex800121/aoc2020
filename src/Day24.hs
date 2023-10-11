module Day24 where

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

type Index = (Int, Int)

data Direction
  = E
  | SE
  | SW
  | W
  | NW
  | NE
  deriving (Show, Eq, Ord)

parseDirection :: String -> [Direction]
parseDirection "" = []
parseDirection ('e' : xs) = E : parseDirection xs
parseDirection ('s' : 'e' : xs) = SE : parseDirection xs
parseDirection ('s' : 'w' : xs) = SW : parseDirection xs
parseDirection ('w' : xs) = W : parseDirection xs
parseDirection ('n' : 'w' : xs) = NW : parseDirection xs
parseDirection ('n' : 'e' : xs) = NE : parseDirection xs

{-

##
###
 ##

-}
readDirection :: Index -> Direction -> Index
readDirection (x, y) d = case d of
  E -> (x + 1, y)
  SE -> (x + 1, y + 1)
  SW -> (x, y + 1)
  W -> (x - 1, y)
  NW -> (x - 1, y - 1)
  NE -> (x, y - 1)

day24 :: IO ()
day24 = return ()
