-- List comprehensions (use a set to create a new set)

allPairs xs = [(y,x) | x <- xs, y<- xs]

myconcat :: [[a]] -> [a]
myconcat xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

mlength xs = sum [1 | _ <- xs]

-- Find values(v) that match a key(k) in a table (t)
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k==k']
-- > find 'x' [('a',1), ('x', 3), ('b',3), ('x', 5)]
-- [3,5]
 
adjPairs xs = zip xs (tail xs)

sorted   :: Ord a => [a] -> Bool
sorted xs = and [ x <= y | (x,y) <- adjPairs xs ]

-- positions     :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
	         where n = length xs - 1
-- try:
-- > positions 3 (3:[0..10]++[1..10]++[3])
