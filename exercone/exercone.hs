

findMax :: [Int] -> Int
findMax [x] = x
findMax (x:xs) = max x (findMax xs)


countFirst :: String -> (Char, Int)
countFirst [x] = (x, 1)
countFirst (a:b:rest)
  | a == b    = (a, 1 + snd (countFirst(b:rest)))
  | otherwise = (a, 1)

