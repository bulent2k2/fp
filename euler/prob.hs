-- sum $ filter (\x -> mod x 3 == 0 || mod x 5 == 0) [3..999]

prob1 n = sum $ filter (\x -> mod x 3 == 0 || mod x 5 == 0) [3..n - 1]

