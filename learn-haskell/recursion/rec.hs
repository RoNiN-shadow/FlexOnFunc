


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Couldn't maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
  
