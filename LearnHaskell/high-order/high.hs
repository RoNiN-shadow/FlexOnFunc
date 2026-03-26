

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let
      smaller = quicksort ( filter (<=x) xs)
      bigger  = quicksort ( filter (>x) xs)
  in smaller ++ [x] ++ bigger

largestDivision :: (Integral a) => a
largestDivision = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n  = n : chain (n `div` 2) 
  | odd n   = n : chain (n*3 +1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where
      isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

containsTree :: [Int] -> Bool
containsTree = foldr (\x rest -> if x == 3 then True else rest) False
