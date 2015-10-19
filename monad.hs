data Tree a = Leaf a | Node (Tree a) (Tree a)

-- sample tree of chars
tree :: Tree Char
-- root -> n1 -> a, b
-- root -> n2 -> n3, n4
-- n3 -> c
-- n4 -> n5, n6
-- n5/n6 -> x/y
tree =  Node (Node (Leaf 'a') (Leaf 'b'))
             (Node (Leaf 'c') (Node (Leaf 'x') (Leaf 'y')))

flatten (Leaf a)   = [a]
flatten (Node l r) = flatten l ++ flatten r

--
-- state transformer is a monad....
--

type State = Int
-- S: dummy constructor!
-- ST: transforms S1 and produces a return value with type a.
--     returns a pair of (a1,S2) where S2 is the transformed state
data ST a = S (State -> (a, State))

-- apply a function as transformer (by simply removing the dummy constr!)
apply        :: ST a -> State -> (a,State)
apply (S f) x = f x

instance Monad ST where
   -- return :: a -> ST a
   return x   = S (\s -> (x,s))
   -- (>>=)  :: ST a -> (a -> ST b) -> ST b
   st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

-- a sample: 

fresh :: ST Int
fresh_original =  S (\n -> (n, n+1))
-- Ex1- Define a function app :: (State -> State) -> ST State, such 
-- that fresh can be redefined by fresh = app (+1).
app :: (State -> State) -> ST State
app f = S (\s -> (s, f s))
fresh = app (+1)

-- define a function that takes a tree as its argument, and
-- returns a state transformer that produces the same tree 
-- with each leaf labelled by a fresh integer:
mlabel            :: Tree a -> ST (Tree (a,Int))
mlabel (Leaf x)   =  do n <- fresh
                        return (Leaf (x,n))
mlabel (Node l r) =  do l' <- mlabel l
                        r' <- mlabel r
                        return (Node l' r')

label  :: Tree a -> Tree (a,Int)
initial_state = 999
label_original t = fst (apply (mlabel t) initial_state)
-- Ex2- Define a function
run :: ST a -> State -> a
-- such that label can be redefined by label t = run (mlabel t) 0.
run (S st) s = fst (st s)
label t = run (mlabel t) initial_state

-- labeled tree:
ltree = label tree

test = putStrLn ("Tree: " ++ (flatten tree) ++
  "\nLabelled: " ++ show(flatten ltree))

{-
*Main> flatten tree
"abcxy"
*Main> flatten ltree
[('a',0),('b',1),('c',2),('x',3),('y',4)]
*Main>
-}

--
main = say_hi >> be_nice

say_hi = putStrLn "Hello World."
be_nice = putStrLn "Nice to meet you!"

main2 = main >> main

main_inf = say_hi >> be_nice >> main_inf
