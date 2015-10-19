Try this out!

Sun, 24 May 2015, 13:06 
docri  	
   Quote   Report    1
As has been posted many times before the general solution is (2n)!/n!^2.  

Just for fun, and because I'm on a mission to learn more about Haskell, I chose to implement a solution based on canceling prime factors from lists. This makes this problem very similar to problem 5 (Smallest multiple). This approach would also work for very limited integer bounds.  This stuff might come in handy later.

Haskell
Hide Code

import Data.List (sort)

-- find all prime factors of n and add them to a list.
primeFactors :: Integer -> [Integer] -> [Integer]
primeFactors 0 ps = ps
primeFactors 1 ps = ps
primeFactors n ps = primeFactors' n 2 ps
  where
    primeFactors' :: Integer -> Integer -> [Integer] -> [Integer]
    primeFactors' n q ps
      | q*q > n = (n:ps)
      | n `mod` q == 0 = primeFactors' (n `div` q)  q (q:ps)
      | q == 2 = primeFactors' n 3 ps
      | otherwise = primeFactors' n (q+2) ps

-- divide two lists of prime factors.  produce an error if the lists can not
-- evenly divided.  for instance:
--  [2,3,3,3,5,5,7,7,11], [3,5,7,7] -> [2,3,3,5,11]
--  [2,2,5,19], [2,3,43] -> error  
dividePrimeFactors :: [Integer] -> [Integer] -> [Integer]
dividePrimeFactors ps [] = ps -- that's ok, division by one
dividePrimeFactors [] qs = error "1/n for n>1 is not an integer." -- undefined
dividePrimeFactors ps qs = dividePrimeFactors' (sort ps) (sort qs) []
  where
    dividePrimeFactors' :: [Integer] -> [Integer] -> [Integer] -> [Integer]
    dividePrimeFactors' [] [] fs = fs
    dividePrimeFactors' [] (q:qs) fs = dividePrimeFactors' [] qs (q:fs)
    dividePrimeFactors' (p:ps) [] fs = dividePrimeFactors' ps [] (p:fs)
    dividePrimeFactors' (p:ps) (q:qs) fs
      | q < p = error "lists are not divisible! duh." -- this should never happen
      | p < q = dividePrimeFactors' ps (q:qs) (p:fs)
      | otherwise = dividePrimeFactors' ps qs fs -- this does the cancelling

-- find the number of possible pathes through an n x n lattice.
-- the result is (2n)!/n!^2. instead of doing it naively, we accumulate
-- all prime factors for [n+1, 2n] and [2..n] and then we do the division
-- by cancelling prime factors through list manipulations. finally, return
-- the product over this list.
--
-- this approach would also work in languages that do not support unbounded
-- integers. 
countPaths :: Integer -> Integer
countPaths n = product pfs
  where
    ps = foldl (\acc n -> primeFactors n acc) [] [n+1..2*n]
    qs = foldl (\acc n -> primeFactors n acc) [] [2..n]
    pfs = dividePrimeFactors ps qs

main = do
  -- the problem with the fancy functions above
  putStrLn $ show $ countPaths 20


And here is the one-line solution:

Haskell
Hide Code

main = do putStrLn $ show $ (product [21..40]) `div` (product [2..20])
