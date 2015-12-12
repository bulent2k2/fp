{- Also see
   ./seq1.hs
   ./seq2.hs
   ./mapm.hs
-}


{- remember a few things from prelude:
   getLine >>= putStr
   do; line <- getLine; putStr $ reverse line
   do; char <- getChar; putStr ("\nGot char:" ++ [char])
-}

-- How to use getChar to define getLine:
-- The idea is to accumulate the input chars in a list
-- So, we use a helper (get) which inputs an empty list to init:
getLine' = get []
get :: String -> IO String
get xs = do
    x <- getChar
    case x of
        '\n' -> return xs
        _ -> get (xs ++ [x])

test1 = do
  line <- getLine'
  putStr $ "\nWe got the following line: " ++ line

interact' f = do
  line <- getLine'
  putStrLn $ f line

test2 = interact' reverse

