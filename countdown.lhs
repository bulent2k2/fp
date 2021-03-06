Countdown example from chapter 11 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


> import System.CPUTime
> import Numeric
> import System.IO

Expressions
-----------

> data Op                       =  Add | Sub | Mul | Div
> 
> valid                         :: Op -> Int -> Int -> Bool
> valid Add _ _                 =  True
> valid Sub x y                 =  x > y
> valid Mul _ _                 =  True
> valid Div x y                 =  x `mod` y == 0
>
> apply                         :: Op -> Int -> Int -> Int
> apply Add x y                 =  x + y
> apply Sub x y                 =  x - y
> apply Mul x y                 =  x * y
> apply Div x y                 =  x `div` y
>
> data Expr                     =  Val Int | App Op Expr Expr
>
> values                        :: Expr -> [Int]
> values (Val n)                =  [n]
> values (App _ l r)            =  values l ++ values r
>  
> eval                          :: Expr -> [Int]
> eval (Val n)                  =  [n | n > 0]
> eval (App o l r)              =  [apply o x y | x <- eval l
>                                               , y <- eval r
>                                               , valid o x y]

Combinatorial functions
-----------------------

BBX. All sub-sets of a list including empty and self. Non-unique elems are considered to be unique! But, if list represents a set, this gives us the power set

> subs                          :: [a] -> [[a]]
> subs []                       =  [[]]
> subs (x:xs)                   =  yss ++ map (x:) yss
>                                  where yss = subs xs
>
> interleave                    :: a -> [a] -> [[a]]
> interleave x []               =  [[x]]
> interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)
> 
> perms                         :: [a] -> [[a]]
> perms []                      =  [[]]
> perms (x:xs)                  =  concat (map (interleave x) (perms xs))

Find all choices from a list, which are given by all possible ways of selecting zero or more elements in any order.
BBX. Given a list, get all possible sub-lists including [] and all permutations ([1,2] != [2,1])
This is truly explosive: 3 elems -> 16 choices. 7 elems -> More than 13k choices! 8 elems -> > 100k. 12 elems, don't do it!

> choices                       :: [a] -> [[a]]
> choices xs                    =  [zs | ys <- subs xs , zs <- perms ys]

Formalising the problem
-----------------------

> solution                      :: Expr -> [Int] -> Int -> Bool
> solution e ns n               =  elem (values e) (choices ns) && eval e == [n]

Brute force solution
--------------------

> split                         :: [a] -> [([a],[a])]
> split []                      = []
> split [_]                     = []
> split (x:xs)                  = ([x],xs) : [(x:ls,rs) | (ls, rs) <- split xs]
>
> exprs                         :: [Int] -> [Expr]
> exprs []                      =  []
> exprs [n]                     =  [Val n]
> exprs ns                      =  [e | (ls,rs) <- split ns
>                                     , l       <- exprs ls
>                                     , r       <- exprs rs
>                                     , e       <- combine l r]
> 
> combine                       :: Expr -> Expr -> [Expr]
> combine l r                   =  [App o l r | o <- ops]
>  
> ops                           :: [Op]
> ops                           =  [Add,Sub,Mul,Div]
> 
> solutions                     :: [Int] -> Int -> [Expr]
> solutions ns n                =  [e | ns' <- choices ns
>                                     , e   <- exprs ns'
>                                     , eval e == [n]]

Combining generation and evaluation
-----------------------------------

> type Result                   =  (Expr,Int)
> 
> results                       :: [Int] -> [Result]
> results []                    =  []
> results [n]                   =  [(Val n,n) | n > 0]
> results ns                    =  [res | (ls,rs) <- split ns
>                                       , lx      <- results ls
>                                       , ry      <- results rs
>                                       , res     <- combine' lx ry]
> 
> combine'                      :: Result -> Result -> [Result]
> combine' (l,x) (r,y)          =  [(App o l r, apply o x y) | o <- ops
>                                                            , valid o x y]
> 
> solutions'                    :: [Int] -> Int -> [Expr]
> solutions' ns n               =  [e | ns'   <- choices ns
>                                     , (e,m) <- results ns'
>                                     , m == n]

Exploiting numeric properties
-----------------------------

> valid'                        :: Op -> Int -> Int -> Bool
> valid' Add x y                =  x <= y
> valid' Sub x y                =  x > y
> valid' Mul x y                =  x /= 1 && y /= 1 && x <= y
> valid' Div x y                =  y /= 1 && x `mod` y == 0
> 
> results'                      :: [Int] -> [Result]
> results' []                   =  []
> results' [n]                  =  [(Val n,n) | n > 0]
> results' ns                   =  [res | (ls,rs) <- split ns
>                                       , lx      <- results' ls
>                                       , ry      <- results' rs
>                                       , res     <- combine'' lx ry]
> 
> combine''                     :: Result -> Result -> [Result]
> combine'' (l,x) (r,y)         =  [(App o l r, apply o x y) | o <- ops
>                                                            , valid' o x y]
> 
> solutions''                   :: [Int] -> Int -> [Expr]
> solutions'' ns n              =  [e | ns'   <- choices ns
>                                     , (e,m) <- results' ns'
>                                     , m == n]

Interactive version for testing
-------------------------------

> instance Show Op where
>    show Add                   =  "+"
>    show Sub                   =  "-"
>    show Mul                   =  "*"
>    show Div                   =  "/"
> 
> instance Show Expr where
>    show (Val n)               =  show n
>    show (App o l r)           =  bracket l ++ show o ++ bracket r
>                                  where
>                                     bracket (Val n) = show n
>                                     bracket e       = "(" ++ show e ++ ")"
> 
> showtime                      :: Integer -> String
> showtime t                    =  showFFloat (Just 3)
>                                     (fromIntegral t / (10^12)) " seconds"
> 
> display                       :: [Expr] -> IO ()
> display es                    =  do t0 <- getCPUTime
>                                     if null es then
>                                        do t1 <- getCPUTime
>                                           putStr "\nThere are no solutions, verified in "
>                                           putStr (showtime (t1 - t0))
>                                      else
>                                        do t1 <- getCPUTime
>                                           putStr "\nOne possible solution is "
>                                           putStr (show (head es))
>                                           putStr ", found in "
>                                           putStr (showtime (t1 - t0))
>                                           putStr "\n\nPress return to continue searching..."
>                                           getLine
>                                           putStr "\n"
>                                           t2 <- getCPUTime
>                                           if null (tail es) then
>                                              putStr "There are no more solutions"
>                                            else
>                                              do sequence [print e | e <- tail es]
>                                                 putStr "\nThere were "
>                                                 putStr (show (length es))
>                                                 putStr " solutions in total, found in "
>                                                 t3 <- getCPUTime
>                                                 putStr (showtime ((t1 - t0) + (t3 - t2)))
>                                     putStr ".\n\n"
> 
> main                          :: IO ()
> main                          =  do hSetBuffering stdout NoBuffering
>				      putStrLn "\nCOUNTDOWN NUMBERS GAME SOLVER"
>                                     putStrLn "-----------------------------\n"
>                                     putStr "Enter the given numbers : "
>                                     ns <- readLn
>                                     putStr "Enter the target number : "
>                                     n  <- readLn
>                                     display (solutions'' ns n)

Choose the correct implementation of a function
removeone :: Eq a => a -> [a] -> [a]
that removes the first occurence of a given element from a list.

> removeone x [] = []
> removeone x (y:ys)
>   | x == y    = ys
>   | otherwise = y:removeone x ys
> test1 = removeone 3 (3:3:5:3:[1..10])
> test2 = removeone 3 (1:2:3:3:5:3:[1..10])

Choose a correct implementation of the function 
isChoice :: Eq a => [a] -> [a] -> Bool
that decides whether one list is chosen from another.
In other words, isChoice xs ys checks whether all elements in xs are present in ys.
Hint: This is NOT set containment!

> isChoice []     _  = True
> isChoice (x:xs) [] = False
> isChoice (x:xs) ys = elem x ys && isChoice xs (removeone x ys)
> test3 = isChoice [1,2,3,1] [3,2,1,1]

does this work?
isChoice [1,1,1] [1..] 

unfortunately, the following does not work:
isChoice [1,2] [1,1..]

