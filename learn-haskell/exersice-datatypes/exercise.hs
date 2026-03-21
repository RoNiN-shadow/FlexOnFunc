

data Nested a = Elem a
              | List [Nested a]

flatten :: Nested a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs
