{- euler problem 10: 
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
  Find the sum of all the primes below two million.
  try tcl (:-)
-}
-- very slow
sum2 n = sum ( primes (n-1) )

-- euler problem 7: find the 10001st prime
-- > take 10001 (primes3) 
-- 104681,104683,104693,104701,104707,104711,104717,104723,104729,104743]
-- So, it's 104743. Take a bit of a time, though..
-- 

factors n = [x | x <- [1..n], mod n x == 0]
prime n = [1,n] == factors n
primes n = [x | x <- [2..n], prime x]
countPrimes n = length (primes n)

{- Find the distances between primes.. this is doing too much:
   for half of all possible pairs! -}
primeGaps0 n = [y-x | x <- primes n, y <- [x+1..n], prime y]
-- How to make pairs out of primes: [(2,3), (3,5), (5,7), ...]
primePairs n = zip ps (tail ps) where
	   ps = primes n
primeGaps n = [y-x | (x,y) <- primePairs n]
check n = length (primeGaps n) == length (primes n) - 1

{-
Bulent> maximum (primeGaps 1000)
20
Bulent> maximum (primeGaps 10000)
36
Bulent> maximum (primeGaps 100000)
72
Bulent>
-}

-- Ancient Mathematician's Sieve (google: ancient math sieve)
-- rather slow at about 500k
primes2 = sieve [2..]
sieve :: [Int] -> [Int]
sieve (p:xs) = p:sieve [x|x<-xs, x `mod` p /= 0]

-- Run-time optimization by starting sieving from square of p
primes3 = sieve2 [2..]
-- sieving to start from square of p, but, need to sieve with each natural!
sieve2 (p:xs) = p:sieve [x|x<-xs, x < p^2 || x `mod` p /= 0]
-- This makes a significant difference, one order of magnitude or more, it seems..
{- in about 12 hours, we reach beyond: 4.8M
9897,4799909,4799923,4799941,4799953,4799957,4799981,4799983,4799987,4799999,4800007,4800023,4800043,4800049,4800053,4800067,4800073,4800083,4800091,4800101,4800113,4800149,4800163,4800193,4800199,4800209,4800221,4800241,4800253,4800281,4800287,4800317,4800347,4800361,4800373,4800401,4800421,4800427,4800431,4800437,4800451,4800487,4800541,4800547,4800557,4800599,4800619,4800623,4800641,4800659,4800661,4800667,4800683,4800703,4800707,4800709,4800737,4800773,4800779,4800781,4800799,4800811,4800827,4800841,4800853,4800857,4800881,4800893,4800911,4800931,4800937,4800941,4800947,4800973,4800989,4801033,4801039,4801051,4801057,4801061,4801099,4801109,4801117,4801133,4801151,4801163,4801169,4801171,4801177,4801183,4801189,4801201,4801207,4801213,4801253,4801267,4801289,4801309,4801339,4801351,4801369,4801393,4801409,4801441,4801451,4801499,4801507,4801513,4801519,4801553,4801561,4801579,4801603,4801607,4801609,4801613,4801627,4801631,4801673,4801679,4801691,4801697,4801711,4801717,4801723,4801729,4801781,4801789,4801793,4801829,4801847,4801861,4801873,4801903,4801921,4801933,4801949,4801969,4801999,4802011,4802047,4802051,4802059,4802071,4802087,4802089,4802093,4802099,4802107,4802123,4802129,4802137,4802153,4802191,4802209,4802219,4802243,4802249,4802257,4802293,4802299,4802321,4802323,4802327,4802333,4802339,4802353,4802393,4802411,4802443,4802461,4802491,4802513,4802519,4802521,4802527,4802531,4802533,4802537,4802543,4802557,4802569,4802599,4802621,4802641,4802657,4802683,4802689,4802723,4802729,4802731,4802741,4802771,4802779,4802783,4802803,4802813,4802843,4802851,4802873,4802879,4802881,4802899,4802911,4802921,4802927,4802939,4802947,4802989,4803031,4803037,4803049,4803053,4803059,4803091,4803103,4803119,4803137,4803151,4803157,4803167,4803173,4803179,4803187,4803193,4803209,4803217,4803223,4803247,4803277,4803283,4803301,4803307,4803317,4803319,4803341,4803347,4803361,4803373,4803391,4803397,4803413,4803427,4803439,4803467,4803479,4803493,4803497,4803509,4803511,4803529,4803563,4803569,4803581,4803583,4803593,4803607,4803641,4803649,4803653,4803677,4803679,4803709,4803719,4803737,4803749,4803767,4803769,4803781,4803787,4803797,4803857,4803871,4803881,4803899,4803907,4803913,4803919,4803923,4803947,4803949,4803959,4803961,4803971,4803983,4803989,4803991,4804021,4804027,4804049,4804061,4804087,4804091,4804109,4804133,4804157,4804159,4804187,4804201,4804213,4804231,4804249,4804253,4804271,4804273,4804277,4804297,4804307,4804333,4804337,4804351,4804361,4804363,4804399,4804409,4804417,4804427,4804439,4804463,4804529,4804567,4804571,4804573,4804577,4804589,4804607,4804619,4804627,4804643,4804661,4804697,4804699,4804703,4804711,4804717,4804733,4804741,4804753,4804759,4804781,4804799,4804823,4804837,4804841,4804867,4804883,4804889,4804937,4804939,4804957,4804979,4804991,4804993,4804997,4805011,4805041,4805063,4805069,4805111,4805149,4805161,4805179,4805189,4805191,4805231,4805249,4805257,4805267,4805287,4805291,4805303,4805357,4805393,4805401,4805419,4805429,4805431,4805443,4805453,4805467,4805513,4805531,4805533,4805539,4805557,4805579,4805587,4805599,4805621,4805629,4805639,4805653,4805657,4805659,4805683,4805693,4805699,4805707,4805711,4805719,4805737,4805743,4805753,4805761,4805791,4805821,4805831,4805861,4805903,4805929,4805939,4805953,4805959,4806001,4806013,4806029,4806071,4806077,4806127,4806161,4806167,4806181,4806187,4806211,4806233,4806283,4806299,4806301,4806313,4806323,4806341,4806367,4806377,4806383,4806391,4806401,4806407,4806409,4806413,4806419,4806421,4806433,4806437,4806443
Interrupted.
-}
