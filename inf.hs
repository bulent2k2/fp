-- Don't use this function! Infinite recursion
f x = x + f x

-- Don't "show" this expression. 
-- [0..]
-- But, can use it :-)
index xs = zip [0..] xs

-- chase its own tail:
loop = tail loop
test1 = fst (1, loop)

ones = 1:ones
test2 = take 10 ones
