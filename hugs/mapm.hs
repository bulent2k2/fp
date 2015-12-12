-- my mapM: take a function, f, and map it over a list of a's: 'as'. 
-- But, f is special: takes an a to an 'm b': an action that returns a 'b'.
-- The result is an action that returns a list of 'bs'
mym :: Monad m => (a -> m b) -> [a] -> m [b]

-- option 1. Works beautifully.
mym f as = sequence (map f as)

{- option 2. Works, too.
mym f [] = return []
mym f (a:as) = f a >>= \b -> mym f as >>= \bs -> return (b:bs)
-}

-- o3. Type error in generator (lines@a:b:c:[])
-- mym f as = sequence_ (map f as)

{- o4. Type error in application. (Unifying [a] with a gives infinite type!)
mym f [] = return []
mym f (a:as) = f a >> \b -> mym f as >> \bs -> return (b:bs) -}

{- o5. Syntax error!
mym f [] = return []
mym f (a:as) = do
  f a -> b
  mym f as -> bs
  return (b:bs) -}

{- o6. Works!
mym f [] = return []
mym f (a:as) = do
  b <- f a
  bs <- mym f as
  return (b:bs)
-}

{- o7. Sure. Works!
mym f [] = return []
mym f (a:as) = f a >>= \b -> do
  bs <- mym f as
  return (b:bs)
-}

{- o8. Reverses the list!
mym f [] = return []
mym f (a:as) = f a >>= \b -> do
  bs <- mym f as
  return (bs ++ [b]) -}

test :: IO String
test = do
  lines@a:b:c:[] <- mym get3 [1,2,3]
  putStrLn c
  putStrLn b
  putStrLn a
  return lines

{- sample:
Main> test
a
b
c
3c
2b
1a

Main> 
-}


-- to test..
-- test = test2 12345
test2 a = do 
  str <- get3 a
  putStrLn str

get3 :: Show a => a -> IO String
get3 x = get2 $ show x

get2 :: String -> IO String
get2 xs = do
    x <- getChar
    case x of
        '\n' -> return xs
        _ -> get2 $ xs ++ [x]


-- Answer: Good options: 1,2,6,7 
