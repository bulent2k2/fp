import Data.List (sort)
import Data.Char (ord)

parse :: String -> [String]
parse xs = case break (==',') xs of
	            (ys,"") -> [ys]
	            (ys,zs) ->  ys : parse (tail zs)

letterVal :: Char -> Int
letterVal '"' = 0
letterVal  c  = ord c - 64

main :: IO ()
main = readFile "./euler22.txt" >>= print . sum . map (uncurry (*)) . zip [1..] . map (sum . map letterVal) . sort . parse


-- var 2

module Main where
import Data.Char
import Data.List(sort)
main = do
  contents <- fmap (flip splitString ',') getContents
  let names = sort $ map toString contents
  print . sum . zipWith (*) [1..] . map nameScore $ names

toString :: String -> String
toString = read
nameScore :: String -> Integer
nameScore  = sum . map toScore 

toScore::Char->Integer
toScore c =
  toInteger (ord c - ord 'A' + 1)
splitString [] _ = []
splitString (x:xs) d
  | d == x = splitString xs d
splitString str d = let (h,t) = break (==d) str in h : splitString t d


-- var 3

sol22 raw =
  let bahan :: [(String,Int)]
      bahan = zip raw [1..]
      refs = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..]
      allInt x = case x of
                      Just x -> x
                      Nothing -> 0
      score (x,sc) = sc * (sum $ map allInt (map (\k -> lookup k refs) x))
  in sum $ map score bahan

readp22 = do
  input <- readFile "p22.txt"
  let tmp = sort $ read input :: [String]
      res = sol22 tmp
  return res



-- var 4

import System.IO
import qualified Data.List as DList
import qualified Data.Char as DChar

main = do
   handle <- openFile "p022_names.txt" ReadMode
   raw_names <- hGetContents handle
   let score = compute_scores 1 $ DList.sort $ words $ map remove_clutter raw_names
   print score
   hClose handle

remove_clutter :: Char -> Char
remove_clutter '\"' = ' '
remove_clutter ',' = ' '
remove_clutter c = c

score :: String -> Int -> Int
score x n  = n * (sum $ map (\y -> (DChar.ord y) - 64) x)

compute_scores :: Int -> [String] -> Int
compute_scores n [] = 0
compute_scores n (x:xs) = (compute_scores (n + 1) xs) + (score x n)
