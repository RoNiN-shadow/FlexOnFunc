countChar :: Char -> String -> Int
countChar c (x:rest)
  |  c == x    = 1 + countChar c rest
  | otherwise  = 0 + countChar x xs

countChar _ [] = 0

removeSpaces :: String -> String
removeSpaces (x:xs)
  | x /= ' ' = x : removeSpaces xs
  | otherwise = removeSpaces xs

removeSpaces [] = []

swapPairs :: String -> String
swapPairs (a:b:rest) = b : a : swapPairs rest

swapPairs [x] = [x]
swapPairs [] = []

myZipStr :: String -> String -> String
myZipStr (x:xs) (y:ys)= x : y : myZipStr xs ys

myZipStr _ _ = []


makePassword :: String -> String
makePassword (x:xs) = '*' : makePassword xs

makePassword [] = []


keepDigits :: String -> String
keepDigits (x:xs)
  | x >= '0' && x <= '9' = x : keepDigits xs
  | otherwise            = keepDigits xs
keepDigits [] = []

multiplyAll :: [Int] -> Int
multiplyAll (x:xs) = x * multiplyAll xs

multiplyAll [] = 1

compressString :: String -> String
compressString (a:b:rest)
  | a == b    = compressString (b:rest)
  | otherwise = a : compressString (b:rest)

compressString [x] = [x]
compressString [] = []


transmute :: String -> String
transmute (x:xs)
  | x == 'd'  = 'g' : transmute xs
  | otherwise = x : transmute xs
transmute [] = []

takeEverySecond :: String -> String
takeEverySecond (a:b:rest) = a : takeEverySecond rest
takeEverySecond [x] = [x]
takeEverySecond [] = []

coutWeak :: [Int] -> Int
countWeak (x:xs)
  | x < 20    = 1 + countWeak xs
  | otherwise = 0 + countWeak xs
countWeak [] = 0

getFirstWord :: String -> String
getFirstWord (x:xs)
  | x == ' '  = []
  | otherwise = x : getFirstWord xs
getFirstWord [] = []
