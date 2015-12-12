module Lab4 where

-- Lab for edx fp101x - Chapter 8 (Interactive Programs)

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle n = n + triangle (n-1)

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count a [] = 0
count a (x:xs)
  | a == x    = 1 + count a xs
  | otherwise =     count a xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

test1 = count 101 (poem >>= \x -> map (ord . \x -> chr (ord x + 4)) x)

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y)
  | x == y    = x
  | x > y     = euclid (x-y,y)
  | otherwise = euclid (x, y-x)

-- ===================================
-- Ex. 3
-- ===================================
-- 
{- EXERCISE 7  (2 points possible)
  Define a recursive function that takes as arguments two functions f and g and a list xs, and
  applies f to all elements at even positions ([0, 2..]) in xs and
  g to all elements at odd positions ([1, 3..]) in xs.
  Example: funkyMap (+10) (+100) [1, 2, 3, 4, 5] = [(+10) 1, (+100) 2, (+10) 3, (+100) 4, (+10) 5].
-}
funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap f g []       = []
funkyMap f g (x:[])   = (f x:[])
funkyMap f g (x:y:[]) = (f x:g y:[])
funkyMap f g (x:xs)   = (f x:funkyMap g f xs)  -- beautiful :-)

test2 length = funkyMap (*10) (*3) $ take length (repeat 1)


-- ex 15,16  LEARN!! What is the type? What does it do??
h  g f = (f.g) $ f
h2 g f = (f.g) f

fix = h fix
fix2 = h2 fix2

-- ex 18/19
f = \f n -> if (n==0) then 1 else n*f(n-1)
k = fix $ f
k3 = fix f
-- LEARN GO FIGURE??
-- k :: Integer -> Integer
-- LEARN: IS POLYMORPHIC AND IS OVERLOADED! WHY??
-- Is NOT hi-order. Is NOT recursive.
test3 = k 42 == 1405006117752879898543142606244511569936384000000000
{- Note:
  *Lab4> let f = \f n -> if (n == 0) then 1 else n * f (n-1)
  *Lab4> :t f
  f :: (Num a, Eq a) => (a -> a) -> a -> a

  But after (doing what?? defining k?) we get:
  *Lab4> :t fix
  fix :: (b -> b) -> b
  *Lab4> :t f
  f :: (Integer -> Integer) -> Integer -> Integer
  *Lab4>
-} 
