-- also see ./fold.hs

filter2lrev p = foldl (\ xs x -> if p x then x:xs else xs) []
filter2l p = foldl (\ xs x -> if p x then xs ++ [x] else xs) []
filter2r p = foldr (\ x xs -> if p x then x:xs else xs) []

-- filter2r only supports infinite lists, partial lists, etc..
-- get first n numbers that meet p
getNatural n p = take n $ filter2r p [1..]
{- e.g., 
   getNatural 10 odd
-}
