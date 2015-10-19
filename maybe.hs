-- the maybe monad! A simple model of failure-prone computations (like division).
-- Q: what is Maybe? A type?
-- A: Well, first of all, it does Not have a type itself! Try :t Maybe. It would fail!
--    Maybe is a parameterized type. 
--    Further, Maybe is an instance of the Monad class of types..
-- Q: what is a class?
--    A collection of somehow "related" types..
-- Q: what is a Monad?
-- A: A class of (parameterized) types is a Monad iff
--    it supports two functions: return and "then" (>>=)
-- Q: what is Just and Nothing?
-- A: The values with type Maybe a. 
--    Nothing represents an exception. It has type:
--      Maybe a. 
--    Just x represents an expected result. It has type:
--      a -> Maybe a
-- Q: Should we think of Just as a function from values of type a to values of type Maybe?
-- A: Type of Just False is Maybe Bool. Type of Just 5 is Num a => Maybe a
--    Try: Just Nothing!
-- Lists are Monads! 
-- List is more general than Maybe in that the "success" value is not necessarily a singular result,
-- but it can be multiple results. 

-- List comprehension is an instance of sequencing (>>=) provided by monads. 
-- Gofer supported comprehension of any monad. Haskell simplified it by 
-- restricting comprehension support to lists only.
-- See the equivalence:
pairs1 xs ys = [(x,y) | x<-xs, y<-ys]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x,y)
