-- module book2.chapter2.simple where

(+++) [] ys = ys
(+++) xs ys = head xs : (tail xs +++ ys)

reverse2 [] = []
reverse2 xs = reverse2 (tail xs) +++ [head xs]

-- Find the min and max in a list
bounds xs = (minimum xs, maximum xs)

-- b2 :: Num b => [a] -> (b,b)
-- b2 []     = (0,0)
b2 []        = error "b2: empty list"
b2 (x:xs) 
 | null xs   = (x,x)
 | otherwise = (min x (fst (b2 xs)), max x (snd (b2 xs)))

test1 = error "bad fun! :-)"