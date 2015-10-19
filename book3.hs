-- parallel and concurrent programming in haskell
{- WORKS in GHCi
let x = 1+2 ::Int
:sprint x
-}

-- p => q == (not p) or q
entails p q 
  | not p     = True
  | q         = True
  | otherwise = False

test1a = succ 9 * 10
test1b = succ (9 * 10)

doubleMe x = x + x
squareMe x = x * x
circle_area r = squareMe r * pi
circle_length r = 2 * pi * r

test2 x = (if x > 100 then x else x*2) + 1

someBasaran = "It's me Basaran. Which one?? :-)"

-- :set prompt "Basaran> "
