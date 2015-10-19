-- hw from chapter 9: Declaring Types and Classes

--ex 0

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero |
           Succ Nat
  deriving Show

-- order does not matter!
--natToInteger (Succ n) = 1 + natToInteger n
--natToInteger Zero = 0

{- works, too, but why do it like this?
natToInteger = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]
-}
natToInteger :: Nat -> Integer
natToInteger = \ n -> genericLength [c | c <-show n, c=='S']
-- need genericLength (overloaded with Num i) as we expect Integer, not Int..
-- natToInteger = \ n -> length [c | c <-show n, c=='S']
test0 = 2 == natToInteger (Succ (Succ Zero))

-- ex 1

-- using -XNPlusKPatterns
{- works
integerToNat 0 = Zero
integerToNat (n+1) = Succ (integerToNat n)
-}
-- BAD! integerToNat n = product [(unsafeCoerce c) :: Integer | c <- show n]
{- works, too
integerToNat (n+1) = Succ (integerToNat n)
integerToNat 0 = Zero
-}
integerToNat (n+1) = let m = integerToNat n in Succ m
integerToNat 0 = Zero

test1 = 5 == natToInteger (integerToNat 5)
test1b = test1 && test0

-- ex2 
add n (Succ m) = Succ (add m n)
add n Zero = n

test2 = 5 == natToInteger (add (integerToNat 2) (integerToNat 3))

-- ex3
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)
test3 = 12 == natToInteger (mult (integerToNat 3) (integerToNat 4))

-- ex4
data Tree1 = Leaf1 Integer | Node1 Tree1 Integer Tree1
  deriving (Show)

occurs :: Integer -> Tree1 -> Bool

occurs m (Leaf1 n) = m == n
occurs m (Node1 l n r) = case compare m n of
  LT -> occurs m l
  EQ -> True
  GT -> occurs m r

tree1 = Node1 (Node1 (Leaf1 1) 2 (Leaf1 3)) 4 (Node1 (Leaf1 5) 6 (Leaf1 7))
test4a = all (\x -> occurs x tree1) [x | x <- [1..7]]
test4b = any (\x -> not (occurs x tree1)) [x | x <- [0,8]]
test4 = test4a && test4b

-- ex 5
data Tree = Leaf Integer | Node Tree Tree deriving (Show)
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balanced (Leaf _) = True
balanced (Node l r) =
  abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- ex 6
balance :: [Integer] -> Tree
halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs) where
  (ys, zs) = halve xs
