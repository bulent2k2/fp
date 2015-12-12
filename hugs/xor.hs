-- xor is exactly like the "or" we use in English. It excludes the middle..
-- i.e., something is true or false, not both, nor neither!
-- You are coming with me, or you are in deep trouble (:-)
-- It is true if only if only one of the arguments is true.

-- What is Bool? It is not a class..
(//) :: Bool -> Bool -> Bool
(//) x y
  |  not (x == y)   = True
  |  otherwise      = False

xor :: [Bool] -> Bool
-- the first definition is not strictly necessary..
xor (x:[]) = x
xor [x,y]  = x // y
xor (x:xs) = x // xor xs

testXor 
  | True  // True  = error "Both args are true"
  | False // False = error "Both args are false"
  | not (True // False) = error "Only first arg is true"
  | not (False // True) = error "Only second arg is true"
  | xor [False] = error "Only one false"
  | xor [False,False] = error "Only two false args"
  | xor [True,True]   = error "Two true"
  | xor [False,False,False] = error "Only three false args"
  | xor [False,False,False,False,False] = error "No true"
  | not (xor [True]) = error "Only true"
  | not (xor [True,False]) = error "Only one true and only one false"
  | not (xor [False,True]) = error "Only one false and only true"
  | not (xor [True,False,False,False,False]) = error "Only the first is true"
  | not (xor [False,False,False,False,True]) = error "Only the last is true"
  | not (xor [False,False,True,False,False]) = error "Only one true"
  | True= error "Show that we hit this pattern. Just checking the test itself!"
  | otherwise = True

-- After loading with :l
-- run the program to execute main:
main = do
  putStrLn ("Running unit tests..\n" ++ (show testXor) ++ "..\nFinished!")
