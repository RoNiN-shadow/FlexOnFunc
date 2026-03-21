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
