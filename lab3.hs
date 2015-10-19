module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, even x]

-- ===================================
-- Ex. 3 - 4 
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares :: ... 
squares :: Integer -> [Integer]
squares n = [x*x | x <- [1..n]]

-- BBX! try 10, 100, 1000, and see the pattern!!!
sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares' :: ...
squares' :: Integer -> Integer -> [Integer]
squares' m n = [x*x | x<-[n+1..n+m]]

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [(m',n') | m'<-[0..m], n'<-[0..n]]

t1 = \x -> \y -> 100 - x
t2 = 3
t3 = \x -> 10000 - x

