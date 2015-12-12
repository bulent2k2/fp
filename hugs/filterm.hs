fM :: Monad m => (a -> m Bool) -> [a] -> m [a]

fM _ [] = return []
fM p (x:xs) = do 
  flag <- p x
  ys <- fM p xs
  if flag then return (x:ys) else return ys
