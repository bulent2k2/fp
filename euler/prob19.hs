nextSunday :: (Int,Int,Int) -> (Int,Int,Int)
nextSunday (d,m,y)
  | elem m [1,3,5,7,8,10,12] = tmonth (d+7, m, y) 31
  | elem m [4,6,9,11] = tmonth (d+7,m,y) 30
  | rem y 4 == 0 = tmonth (d+7,m,y) 29
  | otherwise = tmonth (d+7,m,y) 28

tmonth :: (Int,Int,Int) -> Int -> (Int,Int,Int)
tmonth (d,m,y) end
  | d <= end = (d,m,y)
  | m == 12 = (d-end, 1, y+1)
  | otherwise = (d-end,m+1,y)

euler19 :: (Int,Int,Int) -> Int
euler19 dates = length $ filter (\ (a,b,c) -> a == 1) $
                takeWhile (\ (a,b,c) -> c < 2001) $
                iterate nextSunday dates

