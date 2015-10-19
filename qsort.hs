-- qsort and many ways to reverse qsort

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a<=x]
    larger  = [b | b <- xs, b>x]


qsort2 [] = []
qsort2 (x:xs) = reverse (qsort (x:xs))


qsort3 [] = []
qsort3 (x:xs) = qsort3 larger ++ [x] ++ qsort3 smaller
  where
    smaller = [a | a <- xs, a<x]
    larger  = [b | b <- xs, b>x || b==x]

qsort4 [] = []
qsort4 (x:xs)
  = reverse
    (reverse (qsort4 smaller) ++ [x] ++ reverse (qsort4 larger))
  where smaller = [a | a <- xs, a<x]
        larger  = [b | b <- xs, b>x || b==x]


    
