import Prelude hiding ((^),length,init,drop,and,concat,(!!))

-- TODO: there is an example on mutual recursion, where??

-- merge sorted lists into one big sorted list
-- TODO: use this to implement merge sort!
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys
{-- Using pattern matching
merge (x:xs) (y:ys)
  | x <= y     = x : merge xs (y:ys)
  | otherwise  = y : merge (x:xs) ys
--}

m ^ 0 = 1
m ^ n = m * m ^ (n-1)

length   :: [a] -> Int
length       []  = 0
length (_ : xs)  = 1 + length xs

init   :: [a] -> [a]
init      [_]  = []
init (x : xs)  = x : init xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n [] = []
drop n (x:xs) = drop (n-1) xs

-- LEARN. Missed this one (got it first, then lost it :-)
and [] = True
and (b:bs)
  | b = and bs
  | otherwise = False

concat [] = []
concat (xs : xss) = xs ++ concat xss

(x : _) !! 0 = x
(_ :xs) !! n = xs !! (n-1)
{-- LEARN. with negative or out-of-bounds index, recursive defn gets:
    *** Exception: rec.hs:(28,1)-(29,26): Non-exhaustive patterns in function !!
Prelude gives:
    *** Exception: Prelude.(!!): negative index
or
    *** Exception: Prelude.(!!): index too large
How does it do that? As follows! --}
xs !! index
  | index < 0          = error "negative index.bbx"
  | index >= length xs = error "index too large.bbx"


fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]
