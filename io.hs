--hw from chapter 8
{- ex 1
Which of the following implementations defines a function
  putStr' :: String -> IO ()
that takes a String as its parameter and writes it to the standard output?

Note: The helper function
  putChar :: Char -> IO ()
takes a character as its parameter and writes it to the standard output.
-}
putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = putChar x >> putStr' xs

{- ex 2
Choose all possible implementations for a function
   putStrLn' :: String -> IO () 
that takes a String parameter and writes it to the
standard output, followed by a newline character.
 -}
putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""
-- + putStrLn' xs = putStr' xs >> putStrLn' ""
-- + putStrLn' xs = putStr' xs >> putChar '\n'
-- + putStrLn' xs = putStr' xs >>= \x -> putChar '\n'
-- - putStrLn' xs = putStr' xs >> \x -> putChar '\n'
-- + putStrLn' xs = putStr' xs >> putStr' "\n"
-- - putStrLn' xs = putStr' xs >> putStrLn' "\n"
-- - putStrLn' xs = putStrLn' xs >> putStr' "\n"
test2 = putStrLn' "Hello, World\nand\n  Evren!"

{- ex 3
Which of the following implementation defines a function
   getLine' :: IO String
that reads a line, up to the first \n character, from the standard input?

Note: The helper function
   getChar :: IO Char
reads a single character from the standard input.
-}
getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs = do 
     x <- getChar
     case x of
       '\n' -> return xs
       _    -> get (xs ++ [x])

{- ex 4
Which of the following implementations defines a function
   interact' :: (String -> String) -> IO ()
that takes as its argument a function of type String -> String, and
reads a line from the standard input, and passes it to this function, 
and then prints the resulting output followed by a newline on the
standard output?
-}

interact' :: (String -> String) -> IO ()
interact' f = do
  input <- getLine'
  putStrLn' (f input)

f_interact xs = reverse xs

test3 = interact' f_interact

{- ex 5
Choose all possible implementations of the function
  sequence_' :: Monad m => [m a] -> m ()
that takes a finite, non-partial, list of non-bottom, monadic values,
and evaluates them in sequence, from left to right, ignoring all
(intermediate) results? -}

-- sequence_' :: Monad m => [m a] -> m ()

{- -1 seq_' [] = return []
      seq_' (m:ms) = m >> \_ -> seq_' ms -}
{- +2 seq_' [] = return ()
      seq_' (m:ms) = (foldl (>>) m ms) >> return () -}
-- LEARN! 3 is wrong. Why?? Is it because, it evaluates right to left?
-- +3 -> -
seq_' ms = foldl (>>) (return ()) ms
{- +4 seq_' [] = return ()
      seq_' (m:ms) = m >> seq_' ms -}
{- +5 seq_' [] = return ()
      seq_' (m:ms) = m >>= \_ -> seq_' ms -}
-- -6 seq_' ms = foldr (>>=) (return ()) ms
-- +7 seq_' ms = foldr (>>) (return ()) ms
-- -8 seq_' ms = foldr (>>) (return []) ms

test4 = seq_' [putChar 'a', putChar '\t', putChar 'b', putChar '\n']

{- ex 6
Choose all possible implementations of the function
  sequence' :: Monad m => [m a] -> m [a]
that takes a finite, non-partial, list of non-bottom, monadic values,
and evaluates them in sequence, from left to right, collecting all
(intermediate) results into a list?
-}

seq2 [] = return []
seq2 (m:ms) = do
 a <- m
 as <- seq2 ms
 return (a:as)

test5 = seq2 [putChar 'a', putChar '\t', putChar 'b', putChar '\n']


{- ex 9
EXERCISE 9  (1 point possible)
Implement the function 
  foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
that takes an accumulation function a -> b -> m a, 
and a seed of type a and left folds a finite, non-partial list of
non-bottom elements of type b into a single result of type m a

Hint: The recursive structure of foldLeftM looks as follows:

  foldLeftM f a [] = ... a ...
  foldLeftM f a (x:xs) = foldLeftM f (... f ... a ... (>>=) ...) xs 

Remember the definition of foldl:
Note: z is of type b! x is of type a
  foldl :: (b -> a -> b) -> b -> [a] -> b
  foldl f z []     = z
  foldl f z (x:xs) = foldl f (f z x) xs

-}
foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f z [] = return z
foldLeftM f z (x:xs) = do
   z' <- f z x
   foldLeftM f z' xs

test9 = foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell"
  >>= \r -> putStrLn r


{- ex 10
Implement the function
 foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
( foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a )
which is like to foldLeftM from the previous exercise, except that it folds a finite,
non-partial list of non-bottom elements of type a into a single monadic value of type m b.

Hint: look up the definition of foldr:

-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
-- Note: z is of type b! x is of type a
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 
-} 

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f z [] = return z
foldRightM f z (x:xs) = do
  z' <- foldRightM f z xs
  f x z'

test10 = foldRightM (\a b -> putChar a >> return (a:b)) [] (show [1,3..10]) 
  >>= \r -> putStrLn r

{- ex 11
Choose all possible implementations that define a function
  liftM :: Monad m => (a -> b) -> m a -> m b
that takes a function of type a -> b and "maps" it over a non-bottom
monadic value of type m a to produce a value of type m b?
-}

liftM f m = do
  x <- m
  return (f x)

-- or:
-- liftM f m = m >>= \a -> return (f a)
