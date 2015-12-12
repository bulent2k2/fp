-- merge sort
-- first merge two sorted lists
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  = if x<=y then x:merge xs (y:ys) else y:merge (x:xs) ys
-- halve a list (not really needed, but nice)
halve xs = splitAt (div (length xs) 2) xs
-- now do sort!
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys,zs) = halve xs
