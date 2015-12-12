i = [Nothing, Just 1, Just 10, Just (-2)]
o = [Nothing, Just (-1), Just 0, Just 0]
check = map test i == o
unit
  | check == True = print "All is well"
  | otherwise     = error "Failed unit test"

f a 
  | a == 1 = -1
  | otherwise = 0

liftM :: Monad m => (a -> b) -> m a -> m b

test a = liftM f a

{- option 1: works
liftM f m = do
  a <- m
  return (f a)
-}
-- option 2: type error?
-- liftM f m = m >>= \a -> f a
-- option 3: works
-- liftM f m = m >>= \a -> return (f a)
-- option 4: type error
-- liftM f m = return (f m)
-- option 5: works. But not correct. TODO. WHY NOT?
liftM f m = m >>= \a -> m >>= \b -> return (f a)
-- option 6: works, too But not correct. TODO. WHY NOT?
-- liftM f m = m >>= \a -> m >>= \b -> return (f b)
-- option 7: type error
-- liftM f m = mapM f [m]
-- option 7: type error
-- liftM f m = m >> \a -> return (f a)

main = unit
