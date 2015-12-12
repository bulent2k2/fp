import Prelude hiding ((&&))

{- Works. 1
True && True = True
_ && _ = False
-}

-- Works. 2
-- a && b = if a then if b then True else False else False

-- wrong
-- a && b = if not (a) then not (b) else True

-- wrong
-- a && b = if a then b

-- works. 3
-- a && b = if a then b else False

a && b = if b then a else False

test = case (True && True, True && False, False && False, False && True) of
  (True, False, False, False) -> "All is well"
  (a, b, c, d) -> "Unit test failed " ++ show((a,b,c,d))

-- interesting :-)
test2 x y = if x then if y then 1 else 2 else 3
