--- HW 1

add (x,y) = x+y
zeroto n = [0..n]
add' x y = x+y
incr3 = add' 3
-- mult x y z = x*y*z

-- hw1
n = a `div` length xs
  where a  = 10
        xs = [1..5]

-- EXERCISE 4  (1 point possible)
-- Given the following examples, give another possible calculation for the
-- result of double (double 2). NOT D. NOT A. NOT C.

-- EX 5 (sum, sum' and sum'' are defined in hugs!)
sum''' [] = 0
sum''' (x:xs) = x + sum''' xs

abs x = if x >= 0 then x else -x


-- HW2
second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f (f x)
f xs = take 3 (reverse xs)

-- HW3 also see ./or.hs and ./and.hs

halve1 xs = splitAt (length xs `div` 2) xs
halve2 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs
halve3 xs = splitAt (div (length xs) 2) xs
halve4 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

safetail1 xs = if null xs then [] else tail xs
safetail2 [] = []
safetail2 (_:xs) = xs
safetail3 xs
  | null xs = []
  | True = tail xs
safetail4 [] = []
safetail4 xs = tail xs
safetail5 = \ xs -> 
  case xs of 
    [] -> []
    (_:xs) -> xs
{- test :: ([a],[a],[a])
   expecting ([2], [], []) -}
-- test = (safetail [1,2], safetail [1], safetail [])

mult = \x -> (\y -> (\z -> x * y * z))

{- two ways to compose functions from functions.. Try:
  foo zeroto 10
-- or 
  foo2 zeroto 10
-}
-- instead of
foo f n = reverse (f n)
-- do:
foo1 f = \n -> reverse (f n)
foo2 f = (reverse . f)

-- nice way to remove a list element by index
remove n xs = take n xs ++ drop (n+1) xs

duplicateAt n xs = take (n + 1) xs ++ drop n xs

-- 
-- misc..
--

-- n+k patterns
isEven 0 = True
isEven 1 = False
isEven (n+2) = isEven n

-- arg capture
len ls@(l:_) = "List '" ++ show ls ++ "' starts with " ++ 
  show l ++ " and is " ++ 
  show (length ls) ++ " items long."
len [] = "List is empty!"


-- function composition (use sparingly!)

odd2 = not . even
odd3 = \x -> not (even x)

-- all even [2,4..20]
all2 p xs = and [p x | x <- xs]
all3 p xs = foldr p True xs
all4 p = foldr p True


-- hw4

divides big small = 0 == big `mod` small
divisors x = [d | d<-[1..x], x `divides` d]
riffle xs ys = concat [[x,y] | (x,y) <- zip xs ys]
--xs = 1:[x+1|x<-xs]
--ys = 0:[x+1|x<-ys]

-- function application has higher precedence than function composition. 
--   See myAll1 vs myAll2
-- For additional understanding, try to experiment with infinite and partial lists and see if you can spot any differences in behaviour for the various implementations. 
--   
myAll = all
myAll1  p = and . map p
myAll2 p = and . (map p)
myAll3 p xs = and . map p $ xs
myAll4 p = not . any (not . p)
myAll5 p xs = foldl (&&) True (map p xs)
myAll6 p = foldr (&&) True . map p
myAny = any
myAny1 p = or . map p
myAny2 p xs = length (filter p xs) > 0
myAny3 p = not . null . dropWhile (not . p)
myAny4 p xs = not (all (\x -> not (p x)) xs)
myAny5 p xs = foldr (\x acc -> (p x) || acc) False xs


-- hw5

--sumsqeven = compose [sum, map (^ 2), filter even]
--compose = foldr (.) id

testCurry (x,y) = (y,x)
curry2 f = \x y -> f (x,y)

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)
int2bin2 0 = []
int2bin2 n = n `mod` 2 : int2bin2 (n `div` 2)
int2bin = unfold (==0) (`mod` 2) (`div` 2)
chop8 = unfold null (take 8) (drop 8)
map3 f [] = []
map3 f (x:xs) = f x : map3 f xs
map4 f = unfold null (f . head) tail
iterate2 f = unfold (const False) id f
-- take 10 $ iterate (*2) 1

{-
  DON'T ADD NEW TESTS BELOW
-}

-- sample list, partial lists, infinite lists
xs = [1..6]
plist = [1,2,3,undefined,5,6,7]
plist2 = (1:undefined)
ilist = [13..]

-- Bottom definition:
--   If the computation will not compile, will not terminate, or will throw an Exception, type in: BOTTOM

-- lab4

-- Q: what is the type of: foldr id ? (foldr partially applied to id!)
-- A: a -> [a -> a] -> a
-- Why?? First remember:
-- 0- output = f input, i.e. f :: input -> output
--    but, we could also sub, e.g.:
--      input = i1 -> i2 and output = o1 -> o2
--    so, we would have f :: i1 -> i2 -> o1 -> o2, etc..
-- 1- f application binds from left: f1 f2 f3 == (f1 f2) f3
-- 2- types bind from right: a -> b -> c == a -> (b -> c)
-- foldr      :: (a -> b -> b) -> b -> [a] -> b
-- id         :: a -> a
-- id is the first arg to foldr, so, (a -> (b -> b)) == a -> a,
-- so because the outputs must have the same type
-- a == b -> b
-- QED!
-- Also note: foldr f z xs   applies f on x and z (x = head xs)
-- id x z  would work only if x has type z -> z

-- Q: what is the type of: flip foldr const
-- foldr      :: (a -> b -> b) -> b -> [a] -> b
-- const      :: x -> y -> x
-- flip foldr :: b -> (a -> b -> b) -> [a] -> b
-- (flip foldr) is partially applied on const:
--      drop the first arg of (flip foldr) and sub:
--      b == x -> y -> x
-- so, we get:
--      a -> (x -> y -> x) -> (x -> y -> x) -> [a] -> (x -> y -> x)
-- simplify:
--      a > (x > y > x) > x > y > x > [a] > x > y > x

{- lab4 q15, q16, q17
 Q: What is the type of: h g f = (f . g) $ f
 A:   h :: t(func-g) -> t(func-f) -> t(output)
    Let g :: a -> b => f :: b -> c, then, we can write:
      h :: (a>b)>(b>c)>t(output)
    Now, we need to figure out t(output)..
    Note: (f . g) :: a -> c, 
    So, t(output) == c
    But, because of $, a == b -> c, (why? because (f.g) inputs f!)  
    Then, sub b>c for a, we get:
      ((b>c)>b)>(b>c)>c
    To make it canonical, rewrite type1 <- b, type2 <- c, and omit the type
     ((1>2)>1) > (1>2) > 2
    The pattern: 121122, + the precedence of application:
      ((12)1)(12)2
    In full:
      ((12)1)((12)2)
    Of course 12 is ambiguous! But, if type 9 is not used, i.e., there are less than 10 types, we are fine! :-) So, we better switch to 0-indexing, and introduce . operator in the sense that it combines what comes before (as in paran) until it sees a space, which also combines what comes before and it ignores (.)s
      01.1 12.2
      a -> b is 01
      a -> a -> a is a -> (a -> a) is 0.00 or 0 00 or 000
      (a->a)->a is 00.0
      (a>b)>(c>d)>e is 01.23.4
  Q: What is the type of fix = h fix?
  A: Fix is a partial application of h, so it expects a function (f):
       h :: t(f) -> t(output)
     Remember, h :: ((12)1)(12)2
     The first arg (g) is used up, so, we get:
       h :: (12)2
     But, g :: (12)1, and because g = fix, 1 == 2!
     Expand: 
       h :: (a->a)->a
  Q: Tell me about "fix"!
  A: 1- fix expects a function as input. Therefore, 
        it is a higher order function.
     2- fix can work on any function that works on any type. Therefore,
        it is a polymorphic function
     3- fix has no type class restrictions. Therefore,
        it is NOT an overloaded function.
     4- fix is defined using itself. Therefore,
        it is a recursive function.
     God is recursive. Universe is recursive. I am recursive.
     => We are One!
  Q: Tell me about k: (see ~/fp/lab4.hs)
       k = fix $ f 
         where f = \g n -> if (n==0) then 1 else n*g(n-1)
     1- k :: Integer -> Integer, Therefore, it is NOT an HOF.
     2- Integer type is polymorphic! Therefore k is a PMF.
     3- Is it overloaded? It looks like it. But, I don't know why!! TODO!
     4- It is defined using two recursive functions (fix and f), but it is not an RF itself!
-}
