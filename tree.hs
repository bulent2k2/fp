-- 
-- 
data Tree a = Leaf 
            | Node (Tree a) a (Tree a) 
  deriving(Show)

repeatTree :: a -> Tree a
repeatTree x = Node t x t
  where t = repeatTree x

