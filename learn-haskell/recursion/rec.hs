


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Couldn't maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
  
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x : replicate' (n-1) x
