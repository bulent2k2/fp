-- from list to list of consecutive elements
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- not finished..
zip2 :: ([a],[b]) -> [(a,b)]
zip2 (xs,ys) = [(x,y) | x <- xs, y <- ys]
