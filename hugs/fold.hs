-- also see ./map.hs
-- also see ./filter.hs

-- fold the list from the right
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f z [] = z
foldr2 f z (x:xs) = x `f` (foldr2 f z xs)
-- If list is empty, result is z, the initial value
-- Note: a list [a] with n+1 elements, in general, is: a0:a1:a2: ... :an:[]
-- Folding [a] from the right using function f with initial value z is
-- equivalent to replacing (:) with f:
--   a0 `f` (a1 `f` (a2 `f` (... (an `f` (z)))))
{- For foldr f z [1..4], we have the following (bottom-up) recursion:

                 f
               1   f
                 2   f
                   3   f
                     4   z               

Note: The first arg of f has type: a, 
      the second arg has type:     b (which could be [a]!)
-}

-- fold from the left
foldl2 :: (b -> c -> b) -> b -> [c] -> b
foldl2 f z [] = z
foldl2 f z (x:xs) = foldl2 f (z `f` x) xs
-- Folding [c] from the left using function f with initial value z is
-- equivalent to:
--   (... (((z `f` a1) `f` a1) `f` a2) ... ) `f` an
--     ^ only open parantheses          ^ more list elems
{- For foldl f z [1..4], we have:

                f
              f   4
            f   3
          f   2
        z   1

Note: The first arg of f has type: b (which could be [a]),
      the second arg has type:     a.

-}

{- TRY:
foldr (:) [] [1..4]
foldl (flip (:)) [] [1..4]
take 10 $ foldr (:) [] [1..]
-}
