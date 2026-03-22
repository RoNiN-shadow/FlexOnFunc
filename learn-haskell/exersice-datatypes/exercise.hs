import Data.Char (toLower, isAlpha)

data Nested a = Elem a
              | List [Nested a]

flatten :: Nested a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs


isPalindrome :: String -> Bool
isPalindrome s = clean s == reverse (clean s)
      where
        clean s = filter isAlpha (map toLower s)

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


data Shape = Circle Double
            | Reactangle Double Double
            | Triangle Double Double Double

area :: Shape -> Double
area (Circle r) = 3.14 * r *r 
area (Reactangle a b) = a * b
area (Triangle a b c) = sqrt (s *(s-a)*(s-b)*(s-c))
      where
        s = (a+b+c) / 2

instance Show Shape where
      show (Circle r) = "Circle with radius " ++ show r
      show(Reactangle a b) = "Reactangle " ++ show a ++ "x" ++ show b
      show (Triangle a b c) = "Triangle " ++ show a ++ " " ++ show b ++ " " ++ show c
