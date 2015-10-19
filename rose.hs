------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a 
root (h :> kids) = h

-- all but the root:
children :: Rose a -> [Rose a]
children (h :> kids) = kids

-- first level children only
children1 :: Rose a -> [a]
children1 (h :> kids) = map root kids

-- n'th level children only
childrenn :: (Num a) => a -> Rose b -> [Rose b]
-- think (;-) DRAW!!
-- childrenn 1 r = children1 r
-- childrenn 2 r = foldl (\c -> childrenn 1 c) [] (children r)
childrenn n r = undefined

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (h :> []) = 1
size (h :> kids) = 1 + sum ( map size kids )

tree1 = 'x' :> map (flip (:>) []) ['a'..'x']
tree2 = 1 :> map (\c -> c :> []) [1..5]

leaves :: Rose a -> Int
leaves (h :> []) = 1
leaves (h :> kids) = sum $ map leaves kids

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

--   fmap f (h :> [])   = f h :> []
instance Functor Rose where
  fmap f (h :> kids) = f h :> (map (fmap f) kids)

-- ex9 
f1 r = fmap head $ fmap (\x -> [x]) r
-- LEARN! :t f1           gives: Functor f => f b -> f b
-- but, :t f1 (1 :> [])   gives: Num b => Rose b 
-- so, :t f1          should be: Rose b -> Rose b

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum n1) (Sum n2) = Sum (n1 + n2)
  
instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product n1) (Product n2) = Product (n1 * n2)

unSum :: Sum a -> a
unSum (Sum a) = a
unProduct :: Product a -> a
unProduct (Product a) = a

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap f rose = fold $ fmap f rose

instance Foldable Rose where
  fold (h:>kids) = h `mappend` foldr mappend mempty (map fold kids)
  
tree3 = 1 :> [2 :> [], 3 :> [4 :> []]]
tree3' = fmap Product tree3

sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

tree4 = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

-- COOL :-)
-- Write functions fsum, fproduct :: (Foldable f , Num a) => f a -> a that compute the sum, respectively product, of all numbers in a foldable data structure.

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum     = unSum . foldMap Sum
fproduct = unProduct . foldMap Product

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

