import Prelude hiding (foldr,foldl)

-- From haskellwiki/Fold
-- Also see hi.hs

-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
-- Note: z is of type b! x is of type a
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 
 
{- Let's see where that definition comes from:

Note the structure in basic recursion:
  sum     [] = 0
  sum (x:xs) = add x (sum xs)

only 0 and add are specific. So, make them params to fold:
  sum = fold add 0

In general: fold f z (f -> add, z -> 0)

Now, substitute that in the original defn for sum above:
  fold f z [] = z
  fold f z (x:xs) = f x (fold f z xs)
-}


-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
-- Note: z is of type b! x is of type a
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- examples..

sum' = foldr (+) 0
product' = foldr (*) 1

-- Also understand (fold f z) as replacing ':' (cons) with f and [] (empty list) with z.

-- is this faster than (++)??
append xs ys = foldr (:) ys xs

anytrue = foldr (||) False
alltrue = foldr (&&) True

doubleall = foldr doubleAndCons []
  where doubleAndCons x xs = (2*x):xs
test1 = doubleall [1..10]

doubleall' = map (*2)
test2 = doubleall' [1..10]

map' f [] = []
map' f (x:xs) = f x:map' f xs


-- !! foldl defined using foldr !!
foldl' f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a 
foldl' f = flip $ foldr (\a b g -> b (f g a)) id 
