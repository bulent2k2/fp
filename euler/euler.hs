-- p4: https://projecteuler.net/thread=4#661
-- palindrome is kind of slow
palindrome = maximum (filter (isPalindrome) [a*b | a <- [100..999], b <- [a..999]])
	where
		isPalindrome a = a == merge (makeList a)
			where
				merge [] = 0
				merge (x:xs) = x + merge (map (\x -> x*10) xs)
				makeList 0 = []
				makeList a = (makeList (div a 10)) ++ [(mod a 10)]

p4 = [m | a <- [9], b <- [0..9], c <- [0..9], m <- [100001* a + 10010 * b + 1100 * c], [x | x <- [100..999], m `mod` x == 0 && m `div` x < 1000] /= []]

-- p3
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?


-- euler problem 1
p1_test1 = sum [n | n <- [1..999], n `mod` 5 == 0 || n `mod` 3 == 0]

p1_test2 = sum [3,6..limit] + sum [5,10..limit] - sum[15,30..limit]
  where limit = 999

-- p1 == multiples
p1 x 
  | x < limit && (mod x 3 == 0 || mod x 5 == 0)  = x + p1 (x+1)
  | x < limit                                    = p1 (x+1)
  | otherwise                                    = 0
    where limit = 1000

-- call as: multiples 0
multiples :: Int -> Int
multiples x 
  | x < bound && (mod x 3 == 0 ||  mod x 5 == 0)    = x + multiples (x+1)
  | x < bound                                       = multiples (x+1)
  | otherwise                                       = 0
  where bound = 1000
