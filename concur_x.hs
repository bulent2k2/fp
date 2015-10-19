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
action :: Concurrent a -> Action
action (Concurrent ma) = ma (\x -> Stop)
stop :: Concurrent a
stop = Concurrent( \f -> Stop )
atom :: IO a -> Concurrent a
atom x = Concurrent(\c -> Atom (x >>= return . c))
fork :: Concurrent a -> Concurrent ()
fork m = Concurrent (\c -> Fork (action m) (c ()))
par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent m1) (Concurrent m2) = Concurrent (\c -> Fork (m1 c) (m2 c))
helper :: Concurrent a -> (a->Action) -> Action
helper (Concurrent m) = m
instance Monad Concurrent where
    (Concurrent f) >>= k = Concurrent (\c -> f (\x -> helper (k x) c))
    return x = Concurrent (\c -> c x)
roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (a:as) = case a of
  Atom (am) -> do
    a' <- am
    roundRobin (as ++ [a'])
  Fork a1 a2 -> roundRobin (as ++ [a1,a2])
  Stop -> roundRobin as
ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

