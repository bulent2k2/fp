import Prelude hiding ((||))

-- My favorite in two different forms:
{- I like the second one!
x || y = case not x == y of 
    True -> True
    False -> x
-}
x || y
  | not x == y = True
  | otherwise  = x

{- good:
False || False = False
_ || _ = True

- good, too 
False || b = b
True || _ = True

- nope
b || c
  | b==c = True
  | otherwise = False

- works, too
b || c
  | b==c = b
  | otherwise = True

- works, four!
b || False = b
_ || True = True

- works, five!
b || c
  | b == c = c
  | otherwise = True

- nope
b || True = b
_ || True = True

- works, sixth and the final time!
False || False = False
False || True = True
True || False = True
True || True = True
-}

-- expecting (False,True,True,True)
test =
  case (False||False, False||True, True||False, True||True) of
    (False,True,True,True) -> "All is well"
    (_,_,_,_) -> "Unit test failed!"
