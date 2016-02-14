
-- infinite loop
loop = tail loop

test1 = fst (1, loop)

-- no need for paran (:-). -> binds to everything on the right hand side..
mult = \x -> \y -> x * y

fibs = 0:1:[x+y | (x,y) <- zip fibs $ tail fibs]

{- already in prelude!!
zip3 xs ys [] = []
zip3 [] ys zs = []
zip3 xs [] zs = []
zip3 (x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ zip3 xs ys zs
-}

{-
Main> take 20 bbFibs 
[0,0,1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136,5768,10609,19513]
Main> take 20 fibs 
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
Main> 
-}
bbFibs = 0:0:1:[x+y+z | (x,y,z) <- zip3 bbFibs (tail bbFibs) (tail $ tail bbFibs)]

-- TODO: 
-- how to find the intersection of bbFibs and fibs??

data Tree a = Leaf |
              Node (Tree a) a (Tree a)

-- see :t repeat

repeatTree :: a -> Tree a
repeatTree x = Node t x t
  where t = repeatTree x

{- TODO: Both of the following work! :-)
takeTree 3 (repeatTree 5)  
takeTree 3 $ repeatTree 5
-- See t1 below
takeTree 10 t1
-- TODO: BUG!
takeTree 5 t1
-- Returns:
> [1,2,4,5]
-}
takeTree :: Integral a => a -> Tree b -> [b]
takeTree 0 Leaf = []
takeTree 0 (Node tl x tr) = []
takeTree 1 Leaf = []
takeTree 1 (Node tl x tr) = [x]
takeTree n Leaf = []
takeTree n (Node tl x tr) = [x] ++ (takeTree half1 tl) ++ (takeTree half2 tr)
  where half1 = div (n - 1) 2
        half2 = n - 1 - half1

t1 = Node tl 1 tr
  where tl = Node Leaf 2 Leaf
        tr = Node (Node Leaf 3 Leaf) 4 (Node Leaf 5 Leaf)
