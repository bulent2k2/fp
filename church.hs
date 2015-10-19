{- A church numeral is a function! Or functional objects, as such they are opaque.. Use "sample", or c2i, or c2s below to show them..
   An abstraction that represents numbers
   compare concrete representations using decimal, binary, Roman, string of *, etc.
   Church (and Peano earlier) took two notions to be fundamental:
     zero (where we start)
     next (go from zero to what is next)
   Any alternatives??
-}



--                successor function
-- type Church = (a -> a) -> a -> a

-- take 0, apply the fun n times to get n

-- If we could do the following: 
{- [zero (+x) y, one (+x) y, two (+x) y]
   where (x,y) = 100,5 -}
-- We would get: [5,105,205]
-- z:=zero, s:=step to get to next using (+s)
-- Now, we can use Integers to sample..
-- sample uses next :: \s -> (+s)
sample z s = [zero (+s) z, one (+s) z, two (+s) z]
test = sample (-5) 7
-- sample 3 4
zero = \s z -> z
one  = \s z -> s z
--two  = \s z -> s (s z)
-- use composition:
--two  = \s z -> (s . s) z
-- use eta reduction:
two  = \s -> s . s

c2i  f = f (+1) 0
c2s  f = f (++"*") ""
c2s2 f = f ('*':) ""

-- use beta reduction:
add x y = \s z -> x s (y s z)
three = add one two

-- sub?? inverse function of add. How?

-- super successor gives us mul(tiplication) i.e., n*m= m additions of n
-- mul x y = \s z -> x (y s) z
--         = \s z -> (x . y) s z
--         = \s -> (x . y) s
mul x y = x . y

-- only if we could recurse? :-) We pretend that we can't go back. Always 'next'ing!!
-- pow x (add one f) = x . pow f

y `plus` x = x + y

pow x n = n x

-- about 50M: 
-- 9*9 <nl> it*it <nl> it*it
ninetosix = pow (three.three) (two.three)
-- fairly quick
quick = c2i ninetosix
ninetoeight = pow (three.three) (pow two three)
-- stack overflow! And pretty quickly, too! Who is stacking and WHERE??
hog = c2i ninetoeight
-- See the good in the hog: Once it dissolves, the memory is cleared, hence abundance is obvious! (:-)

{-
*Main> c2i (pow two two)
4
*Main> c2i (pow two three)
8
*Main> c2i (pow three three)
27
*Main> c2i (pow (three.three) three)
729
*Main> c2i (pow (three.three) (three.two))
531441
*Main> c2i (pow (three.three) (three.three))
*** Exception: stack overflow
*Main>
-}
