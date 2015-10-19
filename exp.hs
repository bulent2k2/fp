import Prelude hiding ((^))

(^) :: (Num a, Eq b, Num b) => a -> b -> a
m ^ 0 = 1
-- m ^ n = m * m ^ (n-1)
m ^ n = m * (^) m (n-1)

unit_test
  | 0 ^ 1  /= 0     = error "Unit test 4 failed."
  | 0 ^ 0  /= 1     = error "Unit test 3 failed."
  | 2 ^ 0  /= 1     = error "Unit test 2 failed."
  | 2 ^ 10 /= 1024  = error "Unit test 1 failed."
  | otherwise       = (0, "Test passed")

-- show (tail unit_test)
