test string = tail string
xs = [4, 5, 6, 1, 4, 1, 3, 5]

last1 xs = drop (length xs - 1) xs
last2 xs = head (drop (length xs- 1) xs)
last3 xs = tail (reverse xs)
last4 xs = reverse (head xs)
last5 xs = xs !! (length xs - 1)
last6 xs = head (drop (length xs) xs)
last7 xs = head (reverse xs)
last8 xs = reverse xs !! (length xs - 1)

init1 xs = tail (reverse xs)
init2 xs = reverse (head (reverse xs))
init3 xs = reverse (tail xs)
init4 xs = take (length xs) xs
init5 xs = reverse (tail (reverse xs))
init6 xs = take (length xs - 1) (tail xs)
init7 xs = drop (length xs - 1) xs


qsort1 [] = []
qsort1 (x : xs) = qsort1 larger  ++ [x] ++ qsort1 smaller
 where smaller = [a | a <- xs, a <= x ]
       larger = [b | b <- xs, b > x ]
