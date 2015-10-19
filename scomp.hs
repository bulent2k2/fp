-- 5.4 string comprehensions

-- Try:
-- > sampleTable "hello, world" 10
sampleTable cs n = zip cs [0..n]

-- count     :: Eq(a) -> [a] -> Int
count c cs = length [c' | c' <- cs, c == c']
-- count_lowers :: String -> Int
isLower c = c >= 'a' && c <= 'z'
count_lowers cs  = length [c | c <- cs, isLower c]
