-- Load into Hugs using:
-- :l hugs/quick-sort.hs

o []     = []
o (x:xs) = o ys ++ [x] ++ o zs
  where  ys = [a | a <- xs, a <= x]
         zs = [a | a <- xs, a >  x]


-- recursive sort in descending order
f []     = []
f (x:xs) = f larger ++ [x] ++ f smaller
  where  larger  = [a | a <- xs, a >= x]
         smaller = [b | b <- xs, b <  x]

-- use to test:
xs = [1..5] ++ [-3..3] ++ [6..10]

-- another impl for descending order
x [] = []
x (y:xs) 
  = reverse
    (reverse (x smaller) ++ [y] ++ (reverse (x larger)))
  where smaller = [a | a <- xs, a <= y]
        larger = [b | b <- xs, b > y]
