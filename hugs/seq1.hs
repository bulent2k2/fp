{- Sequencing.. Act on a list of actions ..

first tested with:
  sequence
and
  sequence_
both of which works :-)
-}
test = mys [putStrLn "Type something:", (getLine>>=putStr), putStrLn "\nWas it printed back to screen?", putStrLn "That's what I thought :-)."]

-- This one returns nothing.
mys :: Monad a => [a b] -> a ()
mys [] = return ()
mys (a:as) = a >> mys as
{- mine. works :-)
mys [] = return ()
mys (a:as) = a >> mys as
-}

{- First option: type error
mys [] = return []
mys (m:ms) = m >> \_ -> mys ms
-}

{- Second option. Works.
mys [] = return ()
mys (m:ms) = (foldl (>>) m ms) >> return ()
-}

-- 3rd option. Inferred type is not general enough!
-- mys ms = foldl (>>) (return ()) ms

{- 4th option. Works.
mys [] = return ()
mys (m:ms) = m >> mys ms
-}

{- 5th option. Works, too.
mys [] = return ()
mys (m:ms) = m >>= \_ -> mys ms
-}

-- 6th option. Type error
-- mys ms = foldr (>>=) (return ()) ms

-- 7th option. Works
-- mys ms = foldr (>>) (return ()) ms

-- 8th option. Type error
-- mys ms = foldr (>>) (return []) ms
