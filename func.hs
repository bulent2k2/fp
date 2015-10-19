import Prelude hiding ((||),(&&))

halve   :: [a] -> ([a],[a])
{-- type error: (/) expects Fractional Int, length returns Int
halve xs = (take n xs, drop n xs)
      where n = length xs / 2
--}
-- halve xs = splitAt (length xs `div` 2) xs
halve xs = (take n xs, drop n xs)
      where n = div (length xs) 2

-- safetail :: [a] -> [a]
-- bad -- safetail [x] = [x]
-- good -- safetail xs = if null xs then [] else tail xs
{-- good
safetail [] = []
safetail (_:xs) = xs
--}
-- lambda and pattern matching!
safetail 
  = \xs -> 
    case xs of
      [] -> []
      (_ : xs) -> xs
	 
{-- good
False || False = False
_ || _ = True
--}
{-- good
False || b = b
True || _ = True
--}
b||c
  | b == c = True
  | otherwise = False

-- good -- a&&b = if a then if b then True else False else False
-- bad a && b = if not (a) then not (b) else True
a && b = if a then b else False


-- mult x y z = x * y * z
-- bad mult ((((\x -> \y) -> \z) -> x * y) * z)
-- ??bad mult = \x -> (\y -> (\z -> x*y*z))
-- bad mult = \x -> (x * \y -> (y * \z -> z))
{--
mult :: Int a => (a -> a -> a -> a)
mult = \x -> (\y -> (\z -> x*y*z))
--}

add  = \x -> (\y -> x+y)
add3 = \x -> (\y -> (\z -> x+y+z))

mult  = \x -> (\y -> x * y)
mult3 = \x -> (\y -> (\z -> x * y * z))

inc = \x -> 1+x

f3 :: Num a => (a -> a) -> a

-- triple of g(3)
f3 g = 3 * g(3)


-- remove the element at pos n from list
remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n+1) xs

