-- Also see ./seq1.hs

-- First tested with prelude's sequence:
test = do
  lines@(x:y:[]) <- mys [getLine, getLine]
  putStrLn $ "Got " ++ show(length lines) ++ " lines:"
  putStrLn $ x ++ "\n -- separator -- \n" ++ y ++ ".\n"

-- Think of Monads as "Actions" as opposed to Functions..
-- a an action (type)
-- a b is an action that returns (type) b
-- mys is a function with input: 
--   a list of actions a returning b.
-- output: an action returning a list of bs
mys :: Monad a => [a b] -> a [b]

{- First option works!
mys [] = return []
mys (a:as) = a >>= \b ->
    do
        bs <- mys as
        return (b:bs)
-}
{- 2nd option. Type error
mys ms = foldr func (return ()) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do
        x <- m
        xs <- acc
        return (x:xs)
-}
{- 3rd option. Inferred type is not general enough
mys ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = m : acc
-}
{- 4th option. Syntax error!
mys [] = return []
mys (m:ms) = return (a:as)
  where
    a <- m
    as <- mys ms
-}
{- 5th option. Works!
mys ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do
      x <- m
      xs <- acc
      return (x:xs)
-}
{- 6th option. Type error
mys [] = return []
mys (m:ms) = m >> \a ->
  do as <- mys ms
     return (a:as)
-}
{- 7th option. Syntax error!
mys [] = return []
mys (m:ms) = m >>= \a -> 
  as <- mys ms
  return (a:as)
-}
-- 8th option. Works.
mys [] = return []
mys (m:ms) = do 
  a<-m
  as<-mys ms
  return (a:as)

-- Options 1,5 and 8 only.

