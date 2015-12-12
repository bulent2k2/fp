-- import Prelude hiding ((and))

-- LEARN!
-- Sets (comp with lists) require check for Equality..
-- But in FP, we want sets of functions..
-- How do you check for equality of functions?
-- Hard! In fact impossible in general 
-- (for some models of computations! yet :-), as we don't know how to
-- solve the halting problem..

double x = x * 2

quadruple x = double ( double(x) )

-- more general range 
--  [3,6..20]
-- but make sure it has only two entries before ..
factorial n = product [1..n]

square x = x * x
cube x = x * square x

diag a b = sqrt ( square a + square b )

average xs = sum xs `div` length xs

add x y = x + y

-- see qsort.hs
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a<=x]
    larger  = [b | b <- xs, b>x]

second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
palindrome xs = reverse xs == xs
twice f x = f (f x)

-- -- my first lambda
-- twice (\x->x+3) 5

-- LEARN!
f a b c = a * b * c
-- The type (e.g., f above)
--   a -> b -> c -> d
-- means: in type defn, associates to right -- CURRYING.. func inputs a type a and returns a function that has type: (b->(c->d))..
--   a -> (b -> (c -> d))
-- LEARN
-- The expression f a b c means (in expr, associates to left)
-- (((f a) b) c)

-- : -> cons (construct list) operator
-- > (:) :: a->[a]->[a]
-- e.g., 
-- > 0:[]

funct x xs = take (x+1) xs ++ drop x xs

f1 n = if mod n 2 == 1
   then 3 * n + 1
   else div n 2

f2 n = f1 (f1 n)
f3 n = f2 (f2 n)
f4 n = f3 (f3 n)
-- !! infinite loop: !!
fx n = fx (fx n)

-- find '!' table
--   where table = (sampleTable "hello, World!" 20)
-- positions ('!', 12) table 
--   where table = (sampleTable "hello, World!" 20)
find k t = [v | (k',v) <- t, k==k']
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
	         where n = length xs - 1
sampleTable cs n = zip cs [0..n]

e3 x = x * 2
e4 (x,y) = x
e6 x y = x * y
e7 (x,y) = (y,x)
e8 x y = (y,x)
e9 [x,y] = (x, True)
e10 (x,y) = [x,y]
e13 x y = x / y
e13b x y = x + y * y
e15 xs ys = (head xs, head ys)

sum100 = sum [x^2 | x <- [1..100]]

pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], 
    sq z == sq x + sq y]
       where sq c = c * c
  
factors n = [x | x <- [1..n], mod n x == 0]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (init(factors num)) == num
{---
*Main> factors 8128
[1,2,4,8,16,32,64,127,254,508,1016,2032,4064,8128]
*Main> factors 496
[1,2,4,8,16,31,62,124,248,496]
*Main> factors 28
[1,2,4,7,14,28]
*Main> factors 6
[1,2,3,6]
*Main>
---}

{--
  > [ (x,y) | x<-"ab", y<-[1..3] ]
  [('a',1),('a',2),('a',3),('b',1),('b',2),('b',3)]
  EQUIVALENT TO
  > concat [ [(x,y)|x<-"ab"] | y<-[1..3] ]
  > [ [(x,y)|x<-"ab"] | y<-[1..3] 
  [ [('a',1),('b',1)] , [('a',2),('b',2)] , [('a',3),('b',3)] ]
--}

scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
  
-- LEARN cons operator (:) and list comprehension is a powerful duo! 
-- (:) :: a -> [a] -> [a] -- Get a head elem, add a tail list..
-- Create some basic lists
--  [1]   === 1:[]
--  [1,2] === 1:[2] == 1:(2:[])
-- Infinite lists using list comprehension!
--   xs = [1..]
--   xs = 1:[x+1 | x <- xs]
-- How does the last one work? is list comprehension a closure? It reads xs
-- and writes to it..

riffle :: [a] -> [a] -> [a]
-- GOOD
riffle xs ys = concat [[x,y] | (x,y) <- zip xs ys]
-- BAD riffle xs ys = [x:[y]|x<-xs, y<-ys]

-- same as factors!
divides x d = mod x d == 0
divisors x = [d | d <- [1..x], x `divides` d]

-- LEARN all :: (a -> Bool) -> [a] -> Bool
test1 = (all (>0) [-3..3] == False , all (>0) [1..5] == True)

-- LEARN why not? aVeryCoolFunc :: (Int a, Float b) => (a->b) -> a -> b
aVeryCoolFunc = undefined

{- LEARN what's going on?? What is the type of foldr id?
   > let what = foldr id
   > :t what
   what :: b -> [b -> b] -> b
   > :t id
   id :: a -> a
   > :t foldr
   foldr :: (a -> b -> b) -> b -> [a] -> b
   > :t foldr id
   foldr id :: b -> [b -> b] -> b

   foldr :: (a -> b -> b) -> b -> ([a] -> b)

-- What is the type of flip foldr const?
   > :t const
   const :: a -> b -> a
   > :t foldr
   foldr :: (a -> b -> b) -> b -> [a] -> b
   > :t flip
   flip :: (a -> b -> c) -> b -> a -> c
   > :t flip foldr const
   flip foldr const
     :: (a -> (a1 -> b -> a1) -> a1 -> b -> a1) -> [a] -> a1 -> b -> a1
With redundant association paranthesis:
     :: (a -> (a1 -> (b -> a1)) -> a1 -> (b -> a1)) -> [a] -> a1 -> (b -> a1)
-}

{-
-- parsers... IO... monads... (category theory)
...
-}

main = do
  a <- getLine
  b <- getLine
  putStrLn $ a ++ b

