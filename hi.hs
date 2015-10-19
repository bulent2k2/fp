-- higher order functions (they operate on or return (other) functions)
-- see: https://www.haskell.org/haskellwiki/Fold

twice = \f -> f . f
-- Multiply by 100 -> twice (*10) 3

-- map f (filter p xs) == [f x | x <- xs, p xs]

-- any or all:
--  :: (a -> Bool) -> [a] -> Bool
-- Five ways to implement "all"
-- Does all elems satisfy the predicate?
-- e.g.,   all (|| True) [True, False]
all1 p xs = and (map p xs)
all2 p = and . map p
all3 p = not . any (not . p)
all4 p xs = foldl (&&) True (map p xs)
allx p = foldr (&&) True . map p
-- LEARN foldr!
{--
test1 = allx even [0,2,4] == all even [0,2,4]
test2 = allx even [0,2,3] == all even [0,2,3]
test3 = test1 && test2 --}

-- Six ways to implement "any"
-- Does any elem satisfy the predicate?
any1 p = or . map p
any2 p xs = length (filter p xs) > 0
any3 p = not . null . dropWhile (not . p)
any4 p xs = not (all (\ x -> not (p x)) xs)
any5 p xs = foldr (\ x acc -> (p x) || acc) False xs
anyx p xs = foldr (||) False (map p xs)

test1 = anyx odd [2,4,6] == any odd [2,4,6]
test2 = anyx odd [1,4,6] == any odd [1,4,6]
test3 = test1 && test2

takeW2 _ [] = []
takeW2 p (x:xs)
 | p x = x : takeW2 p xs
 | otherwise = []

-- NOT QUITE, BUT INTERESTING:
takeW3 p = foldl (\ acc x -> if p x then x : acc else acc) []

dropW2 _ [] = []
dropW2 p (x:xs)
 | p x = dropW2 p xs
 | otherwise = x:xs

-- DROPS ALL!
dropW3 p = foldr (\x acc -> if p x then acc else x : acc) []

{-- TYPE ERROR: WHY??
dropW4 p = foldr add []
  where add [] x = if p x then [] else [x]
        add acc x = x : acc
--}

-- map! We could call it "apply" too. Apply a function to a list
-- also reversed the list:
pam2 f = foldl (\ xs x -> f x : xs) []
-- LEARN: how does this work??
-- Note: [] ++ [1..3] -> [1..3]
-- folding from the left, i.e., 
-- the initial value (null in this case) comes in as xs
-- and is combined with [f x] to feed into the 
-- next level of folding
map2 f = foldl (\ xs x -> xs ++ [f x]) []
-- or:
map3 f = foldr (\ x xs -> [f x] ++ xs) []
-- NOT! Creates a list of lists! concat (++) unwraps [f x] 
mapx f = foldr (\ x xs -> [f x]:xs) []
-- works, too
map4 f = foldr (\ x xs -> (f x):xs) []
-- try:
-- map (+10) (0:(1:(2:[])))

-- filter!
-- try: filter even [0..10]
f2 :: (a -> Bool) -> [a] -> [a]
-- NOTE: think xs as the tree we already process, or accummulated..
-- We are done with xs, now we are adding on x!
f2 p = foldr(\ x xs -> if p x then [x] ++ xs else xs) []
f3 p = foldr(\ x xs -> if p x then x:xs else xs) []
f4 p = foldl(\ xs x -> if p x then xs ++ [x] else xs) []

dec2int :: [Integer] -> Integer
-- [2..5] -> 2345
-- [] -> 0
-- [0..] -> 0

dec2int = foldl(\ x y -> 10 * x + y) 0

-- bad type! symsqeven = compose [sum, map (^ 2), filter even]
-- doesn't even typecheck :-)

compose :: [a->a] -> (a->a)
compose = foldr (.) id

-- curry and uncurry
--   curry sum2 2 5
--   uncurry (curry sum2) (10,20)
--   sum2 (2,5)
sum2 (x,y) = x + y
curry2 f = \ x y -> f (x,y)
uncurry2 f = \ (x,y) -> f x y
test4 = uncurry2 (curry2 sum2) (111,222)

-- a recursive pattern !! --
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)
c2 = unfold null (take 8) (drop 8)
map5 f = unfold null (f . head) tail

iterate2 f = unfold (const False) id f
test5a = [x | x <- iterate (*2) 1, x < 100]
test5b = [x | x <- iterate2 (*2) 1, x < 100]

test6 x = x > 3
