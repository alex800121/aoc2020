{-# LANGUAGE LambdaCase #-}

module Day20 where

import Data.Array qualified as A
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..), FiniteBits)
import Data.Char (isNumber)
import Data.Either (partitionEithers)
import Data.List (foldl', sort, tails)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.WideWord (Word128)
import Data.Word (Word16, Word8)
import Debug.Trace
import MyLib (pickNSplit, toIndex)
import Paths_AOC2020 (getDataDir)

type Index = (Int, Int)

type Pic = Vector Word8

type Tile = (Pic, Edges)

type NTile = (Int, Tile)

type Edges = Vector Edge

type Edge = Word16

run :: [NTile] -> [(Index, NTile)]
run (x : xs) = go [] [((0, 0), x)] xs
  where
    go acc xs [] = xs <> acc
    go acc [] _ = acc
    go acc (x : xs) ys = go (x : acc) (xs <> b) a
      where
        (a, b) = partitionEithers $ map (match x) ys

match :: (Index, NTile) -> NTile -> Either NTile (Index, NTile)
match ((x, y), (_, (_, redge))) t@(n, (tpic, tedge)) = maybe (Left t) Right (listToMaybe ijk)
  where
    ijk =
      [ (bimap (+ x) (+ y) $ toIndex (toEnum i0), (n, (rotatePic 8 rot tpicFlipped, rotateEdges rot tedgeFlipped)))
        | (i0, e0) <- zip [0 ..] $ V.toList redge,
          (tpicFlipped, tedgeFlipped) <- [(tpic, tedge), (flipPic tpic, flipEdges tedge)],
          (i1, e1) <- zip [0 ..] $ V.toList tedgeFlipped,
          reverseWord 10 e0 == e1,
          let rot = i1 - i0 - 2
      ]

flipPic :: (V.Storable a) => Vector a -> Vector a
flipPic = V.reverse

rotateEdges :: Int -> Edges -> Edges
rotateEdges 0 v = v
rotateEdges 1 v = V.backpermute v (V.fromList [1, 2, 3, 0])
rotateEdges 2 v = V.backpermute v (V.fromList [2, 3, 0, 1])
rotateEdges 3 v = V.backpermute v (V.fromList [3, 0, 1, 2])
rotateEdges n v = rotateEdges (n `mod` 4) v

rotatePic :: (V.Storable a, FiniteBits a, Num a) => Int -> Int -> Vector a -> Vector a
rotatePic l 0 v = v
rotatePic l 1 v = V.generate l f
  where
    f i = V.foldl' (\acc x -> if x `testBit` i then (acc `shiftL` 1) `setBit` 0 else acc `shiftL` 1) 0 v
rotatePic l 2 v = V.reverse (V.map (reverseWord l) v)
rotatePic l 3 v = V.generate l f
  where
    f i = V.foldr (\x acc -> if x `testBit` (l - 1 - i) then acc * 2 + 1 else acc * 2) 0 v
rotatePic l i v = rotatePic l (i `mod` 4) v

flipEdges :: Vector Word16 -> Vector Word16
flipEdges = (`V.backpermute` V.fromList [2, 1, 0, 3]) . V.map (reverseWord 10)

readInput :: String -> NTile
readInput s = (n, (pic, edges))
  where
    x : xs = lines s
    n = read (filter isNumber x)
    edges =
      V.fromList
        [ fromChar (head xs),
          fromChar (map last xs),
          fromChar (reverse (last xs)),
          fromChar (reverse (map head xs))
        ]
    pic = V.fromList $ map (fromChar . init . tail) $ init $ tail xs

reverseWord :: (Num a, FiniteBits a) => Int -> a -> a
reverseWord l x = f 0 (l - 1) x
  where
    f acc l x
      | x == 0 || l < 0 = acc
      | x `testBit` 0 = f (acc `setBit` l) (pred l) (x `shiftR` 1)
      | otherwise = f acc (pred l) (x `shiftR` 1)

fromChar :: (Integral a) => String -> a
fromChar = foldl' (\acc -> (\case '#' -> acc * 2 + 1; '.' -> acc * 2)) 0

dragon =
  map (foldl' (\acc -> \case '#' -> (acc * 2) + 1; _ -> acc * 2) (0 :: Word128)) $
    lines "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "

fromPic :: (V.Storable a, Bits a) => Int -> Vector a -> String
fromPic n = unlines . map (fromInt n "") . V.toList

fromInt 0 acc _ = acc
fromInt n acc x = fromInt (pred n) (if x `testBit` 0 then '#' : acc else '.' : acc) (x `shiftR` 1)

buildImage :: [(Index, NTile)] -> (Int, Vector Word128)
buildImage xs = (a, f (V.replicate 8 0) minx miny)
  where
    ix = map fst xs
    b@((minx, miny), (maxx, maxy)) = foldl' (\((a, b), (c, d)) (x, y) -> ((min a x, min b y), (max c x, max d y))) ((0, 0), (0, 0)) ix
    a = product [i | (ix, (i, _)) <- xs, ix `elem` [(x, y) | x <- [minx, maxx], y <- [miny, maxy]]]
    w = A.array b [(ix, c) | (ix, (_, (c, _))) <- xs]
    f acc x y
      | y > maxy = V.replicate 0 0
      | x > maxx = acc <> f (V.replicate 8 0) minx (succ y)
      | otherwise = f (V.zipWith (\a b -> (a `shiftL` 8) .|. fromIntegral b) acc (w A.! (x, y))) (succ x) y

calcDragons :: Int -> Vector Word128 -> Int
calcDragons l pic = np - nd * length pics
  where
    pics =
      [ ()
        | n <- [0 .. 3],
          f <- [id, flipPic],
          let p = V.toList $ rotatePic (8 * l) n $ f pic,
          p' <- tails p,
          n <- [0 .. (8 * l - 1)],
          dragon == zipWith (.&.) dragon (map (`shiftR` n) p')
      ]
    nd = sum $ map popCount dragon
    np = V.sum $ V.map popCount pic

day20 :: IO ()
day20 = do
  input <- map readInput . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  -- input <- map readInput . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/test20.txt"))
  let (a, b) = buildImage $ run input
  putStrLn
    . ("day20a: " ++)
    . show
    $ a
  putStrLn
    . ("day20b: " ++)
    . show
    $ calcDragons 12 b

fromEdge :: Edge -> String
fromEdge = fromInt 10 ""

fromEdges :: Edges -> [String]
fromEdges = map (fromInt 10 "") . V.toList
