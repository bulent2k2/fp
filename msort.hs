msort [] = []
msort [x] = [x]
msort xs = merge (msort h1) (msort h2)
  where (h1,h2) = halve xs

{-- Definition for the function 
  msort :: Ord a => [a] -> [a] 
that implements merge sort, in which the empty list and singleton lists
are already sorted, and any other list is sorted by merging together the
two lists that result from sorting the two halves of the list
separately. The solutions can use the function merge from the previous
exercise and the function halve that splits a list into two halves whose
lengths differ by at most one. --}

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

{-- Definition for the function
  merge :: Ord a => [a] -> [a] -> [a]
that merges two sorted lists in ascending order to give a single sorted list in ascending order. For example:
> merge [2, 5, 6] [1, 3, 4]
[1, 2, 3, 4, 5, 6]  
--}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys
