module Day4 where

import Data.List ((\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust)
import MyLib (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, hexDigitChar, string)

requiredFields :: [String]
requiredFields =
  [ "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  ]

valid :: [String] -> Bool
valid = (&&) <$> (null . (requiredFields \\) . map (take 3)) <*> all (f (choice [byr, iyr, eyr, hgt, hcl, ecl, pid, cid]))
  where
    f x = fromMaybe False . parseMaybe x

byr, iyr, eyr, hgt, hcl, ecl, pid, cid :: Parser Bool
byr = do
  string "byr:"
  a <- read @Int <$> count 4 digitChar <* eof
  return $ a >= 1920 && a <= 2002
iyr = do
  string "iyr:"
  a <- read @Int <$> count 4 digitChar <* eof
  return $ a >= 2010 && a <= 2020
eyr = do
  string "eyr:"
  a <- read @Int <$> count 4 digitChar <* eof
  return $ a >= 2020 && a <= 2030
hgt = do
  string "hgt:"
  a <- read @Int <$> many digitChar
  b <- string "in" <|> string "cm"
  eof
  if b == "in"
    then return $ a >= 59 && a <= 76
    else return $ a >= 150 && a <= 193
hcl = do
  string "hcl:"
  char '#' >> count 6 hexDigitChar >> eof >> return True
ecl = do
  string "ecl:"
  choice (map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) >> eof >> return True
pid = do
  string "pid:"
  count 9 digitChar >> eof >> return True
cid = do
  string "cid" >> takeRest >> return True

day4 :: IO ()
day4 = do
  input <- map words . splitOn "\n\n" <$> readFile "input/input4.txt"
  let fields = map (map (takeWhile (/= ':'))) input
  putStrLn $ ("day4a: " ++) $ show $ length $ filter (null . (requiredFields \\)) fields
  putStrLn $ ("day4b: " ++) $ show $ length $ filter valid input
