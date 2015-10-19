import Data.List.Split

maxcrunch (x:y:xs) = max x y:maxcrunch (y:xs)
maxcrunch _        = []

triangle :: [[Int]] -> Int
triangle [[x]]       = x
triangle (xs:ys:xss) = triangle (zipWith (+) (maxcrunch xs) ys:xss)

main = interact((++"\n").show.triangle.reverse.map(map read.splitWhen (==' ')).filter(/=[]).splitWhen(=='\n'))

-- second one

Haskell:

{-
  The basic idea here is that by starting from the bottom and taking init and
  tail of it, adding them to the row above, then getting the maximum value for each cell;
  Repeat until all the rows are added - will end up with just one cell that has the maximum.
  We can even remember all the numbers along the way.
-}

rows :: [[Int]]
rows = [[75],
        [95, 64],
        [17, 47, 82],
        [18, 35, 87, 10],
        [20, 04, 82, 47, 65],
        [19, 01, 23, 75, 03, 34],
        [88, 02, 77, 73, 07, 63, 67],
        [99, 65, 04, 28, 06, 16, 70, 92],
        [41, 41, 26, 56, 83, 40, 80, 70, 33],
        [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
        [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
        [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
        [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
        [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
        [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

addRow :: [([Int], Int)] -> [([Int], Int)] -> [([Int], Int)]
addRow = zipWith (\ (x, y) (x', y') -> (x ++ x', y + y'))

maxf :: Ord b => (a, b) -> (a, b) -> (a, b)
maxf = \ (x,y) (x',y') -> if y > y' then (x,y) else (x',y')

maxRow :: [([Int], Int)] -> [([Int], Int)] -> [([Int], Int)]
maxRow = zipWith maxf

addUp :: [[Int]] -> [([Int], Int)]
addUp = foldr1 (\ a b -> maxRow (addRow a (init b)) (addRow a (tail b))) . map (map (\x -> ([x], x)))

maxPath :: [([Int], Int)] -> ([Int], Int)
maxPath = foldr1 maxf 

main = (print . maxPath . addUp) rows

time ./problem18
([75,64,82,87,82,75,73,28,83,32,91,78,58,73,93],1074)

real	0m0.006s
user	0m0.002s
sys	0m0.003s
