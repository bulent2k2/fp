
test :: [Int] -> IO ()
test xs
 | length xs == 1 = case head xs of
    0 -> print "List has only one element which is zero."
    1 -> print "List has only one element which is one."
    otherwise -> print $ "List has only one element and it is " ++ (show $ head xs)
 | length xs > 10 = print $ "List has a lot of (actually " ++ show (length xs) ++ ") elements. First 10 are: " ++ show (take 10 xs)
 | length xs > 1 = print $ "Function 'test' is applied on list: " ++ show xs
 | otherwise     = print "Empty list!"

