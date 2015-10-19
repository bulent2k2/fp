SEE concur_x.hs for latest..

module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

{- 
Given a computation of type Action, a function that uses a continuation with result type a has the following type: (a -> Action) -> Action.
This type can be read as a function that takes as input a
  continuation function := (a -> Action)
that specifies how to continue once the result of type a of the current computation is available.
An application f c of this type will call c with its result when it becomes available. 
-}

-- ===================================
-- Ex. 0
-- ===================================
{- To express the connection between an expression of type Concurrent a and one of type Action,
define 'action' that transforms a continuation into an Action that uses Stop :: Action to create
the continuation to the first argument to this func. -}
action :: Concurrent a -> Action
action (Concurrent ma) = ma (\x -> Stop)
action' :: ((a -> Action) -> Action) -> Action
action' ma = ma (\x -> Stop)

-- ===================================
-- Ex. 1
-- ===================================
{- To make the constructors of the data type Action easily accessible, we can define helper functions that hide the boilerplate required to use them.

The first helper function that we will define is the function stop :: Concurrent a, which discards any continuation, thus ending a computation.

Thus we need to return a function of type ((a -> Action) -> Action) wrapped in the Concurrent data type. This function takes a continuation, which gets discarded, and then it returns a Stop action. -}

stop :: Concurrent a
stop = Concurrent( \f -> Stop )

-- ===================================
-- Ex. 2
-- ===================================
{- Now we can define the helper function atom :: IO a -> Concurrent a,
which turns an arbitrary computation in the IO Monad into an atomic action represented using the Atom constructor. The easiest way to implement this function is to first implement
  atom :: IO a -> ((a -> Action) -> Action)
by taking a value
  x :: IO a
and returning a value of type
  ((a -> Action) -> Action)
You already know, from the previous homework and labs, how to combine a value of type IO a
and a function of type a -> IO b into a value of type IO b using (>>=), in this case b is 
instantiated to Action.
  m >>= f  :: IO a -> (a-> IO Action) -> IO Action
You also know how to convert a value of type Action into a value of type IO Action using return. 
  return Action -> IO Action
Finally, the obvious choice to turn a value of type IO Action into an Action is by using the
Atom constructor. 
  Atom (IO Action ) -> Action -}
atom' :: IO a -> ((a -> Action) -> Action)
-- Note: c :: (a->Action)
atom' x = \c -> Atom (x >>= return . c)
atom :: IO a -> Concurrent a
atom x = Concurrent(\c -> Atom (x >>= return . c))

-- ===================================
-- Ex. 3
-- ===================================
{- In order to access Fork, we need to define two operations. The first, called
    fork :: Concurrent a -> Concurrent ()
forks its argument by turning it into an action and continues by passing () as
the input to the continuation -}

fork :: Concurrent a -> Concurrent ()
fork (Concurrent c) = Concurrent(\f -> (action' c))
fork' :: ((a -> Action) -> Action) -> ((() -> Action) -> Action)
fork' ma = \f -> (action' ma) 

{- The second, par :: Concurrent a -> Concurrent a -> Concurrent a, combines two
computations into one by forking them both and passing the given continuation to both parts. -}
par :: Concurrent a -> Concurrent a -> Concurrent a
par = error "You have to implement par"
--par' :: ((a -> Action) -> Action) -> ((a -> Action) -> Action) -> ((a -> Action) -> Action)
--par' m1 m2 = \cont -> fork' m1

-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = error "You have to implement >>="
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = error "You have to implement roundRobin"

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

