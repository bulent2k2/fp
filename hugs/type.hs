-- HW 9 - Declaring Types and Classes

module Main (main) where
import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

data Nat = Zero | 
           Succ Nat
  deriving Show

{- Good!
n2i Zero = 0
n2i (Succ n) = 1 + n2i n
-}
-- Works, too. What's going on? Got it! Beautiful..
n2i = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1: m n)]]
--
{- works! -}
-- n2i :: Nat -> Integer
-- n2i = \n -> genericLength [c | c <- show n, c == 'S']
{- type declaration is needed!! What's going on??
   Unresolved top-level overloading. 
   Binding             : n2i
   Outstanding context : Show c
-}
-- type error, unless we use Int instead of Integer
-- n2i :: Nat -> Int
-- n2i = \n -> length [c | c <- show n, c == 'S']

{- This works
i2n 0 = Zero
i2n (n+1) = Succ (i2n n)
-}
{- Works, too. But how?? TODO
i2n (n+1) = Succ (i2n n)
i2n 0 = Zero
-}
i2n (n+1) = let m = i2n n in Succ m
i2n 0 = Zero

-- this is my first i2n (:-)
n :: Integer -> Nat
n i 
 | i == 0    = Zero
 | otherwise = Succ (n (i-1))

nz = Zero
n1 = Succ Zero
n2 = Succ n1
n3 = Succ n2
test = print (map n2i [nz,n1,n2,n3])
test2 = map n [0..]

-- test:
main = do
--  l <- getLine
--  putStrLn $ reverse l
  test
  print $ take 20 $ map n2i $ map i2n [0..]
  print $ map n2i [add (i2n 10)$i2n 20, add (i2n 0)$i2n 0, add (i2n 0)$i2n 1]
  print $ map n2i [mult (i2n 0)$i2n 4, mult (i2n 3) (i2n 8)]
  print $ map (flip occurs t1) [1,2,3,4,5,6,7,0,8]

{- works
add Zero n = n
add (Succ m) n = Succ (add n m)
-}
{- works
add (Succ m) n = Succ (add n m)
add Zero n = n
-}
-- works
add Zero n = n
add (Succ m) n = Succ (add m n)
--
-- All other variations work, too! --

mult m Zero = Zero
mult m (Succ n) = add m (mult m n)


data Tree = Leaf Integer | 
            Node Tree Integer Tree

occurs :: Integer -> Tree -> Bool
{- works
occurs m (Leaf n) = m == n
occurs m (Node l n r) = case compare m n of
  LT -> occurs m l
  EQ -> True
  GT -> occurs m r
-}
occurs m (Leaf n) = m == n
occurs m (Node l n r) 
  | m == n = True
  | m < n  = occurs m l
  | True   = occurs m r

t1 = Node left 4 right
  where left = Node (Leaf 1) 2 (Leaf 3)
        right = Node (Leaf 5) 6 (Leaf 7)

data Tree2 = Leaf2 Integer | 
             Node2 Tree2 Tree2
balanced :: Tree2 -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r) = 
  abs (leaves l - leaves r) <= 1 && balanced l && balanced r
    where leaves (Leaf2 _) = 1
          leaves (Node2 l r) = leaves l + leaves r

t2 = Node2 left right
  where left = Node2 l2 (Leaf2 1)
        l2 = Leaf2 1
        right = Node2 l3 (Leaf2 1)
        l3 = Node2 l4 (Leaf2 1)
        l4 = Node2 (Leaf2 1) (Leaf2 1)

balance :: [Integer] -> Tree2
balance [x] = Leaf2 x
balance xs = Node2 (balance ys) (balance zs)
  where (ys, zs) = halve xs
        halve xs = splitAt (length xs `div` 2) xs
        
data Answer = Yes | No | Unknown

answers :: [Answer]
answers  = [Yes, No, Unknown]

flp    :: Answer -> Answer
flp Yes = No
flp  No = Yes
flp Unknown = Unknown

isYes Yes = True 
isYes No = False

-- MONOIDS: Algebraic structures over a type a with:
-- a neutral element, and a 
-- binary operation (required to be associative)

class Monoid a where
  mempty :: a
  (<>) :: a -> a -> a

instance Monoid [a] where 
  mempty = []
  (<>) = (++)

-- FUNCTORS: Type constructors (aka constructor class).. 
-- Given a type a, constructs a new type with one operation
-- called fmap:
class MyFunctor f where
  myFmap :: (a->b) -> f a -> f b

-- We want fmap to have two properties, though:
-- 1) Distributive over function composition:
--   (fmap f) . (fmap g) = fmap (f.g)
-- 2) Unitive:
--   fmap id = id
--   (Note: id :: a -> a)

data MyMaybe a = MyNothing | MyJust a

instance MyFunctor MyMaybe where
  myFmap _ MyNothing = MyNothing
  myFmap f (MyJust a) = MyJust (f a)

test3 :: Num a => MyMaybe a -> a
test3 MyNothing = 0
test3 (MyJust n) = n

testf n = n*n*n*n*n

test4 :: Integer -> Integer
test4 n = test3 ( myFmap testf (MyJust n) )

-- Ex 13:

-- Type class "Foldable" is defined in Data.Foldable.
-- Let's re-define it :-)
class (MyFunctor f) => Foldable f where
  fold :: (Monoid m) => f m -> m

data List a = Nil | Cons a (List a)
test5 = Cons 0 $ Cons 1 $ Cons 2 $ Nil

test6 :: Num a => List a -> [a]
test6 (Cons n Nil) = [n]
test6 (Cons n ns) = n : test6 ns

test7 = test6 test5

-- TODO? can't build this instance! Why not?
{-
instance Foldable List where
  fold = foldr (<>) mempty
-}
