flm :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
flm f z [] = return z
flm f z (x:xs) = do 
  a <- f z x
  flm f a xs

testl = flm (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r

frm :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
frm f z [] = return z
frm f z (x:xs) = do
  b <- frm f z xs
  f x b

testr = frm (\a b -> putChar a >> return (a:b)) [] (show [1,3..10]) >>= \r -> putStrLn r
