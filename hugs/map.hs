-- also see ./fold.hs and ./test.hs

-- First understand the following two

-- 1- identity function on lists == shallow copy in Lisp parlance
id2 = foldr (:) []

-- 2- reverse a list.. Note: need to flip args to f
reverse2 = foldl (flip (:)) []



mapAndReverseR f = foldr (\ x xs -> xs ++ [f x]) []

-- fails type check. In two (equivalent?) ways:
-- 1: f x is not a list!
-- 2: map2 :: (a -> [b]) -> [a] -> [b] /= map :: (a->b) -> [a] -> [b]
-- map2 f = foldr (\x xs -> f x ++ xs) []

mapAndReverseL f = foldl (\ xs x -> f x : xs) []

map2 f = foldl (\xs x -> xs ++ [f x]) []


xs = [1..10]
f = \x -> x^2 + 1
