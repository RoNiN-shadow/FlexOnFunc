import Data.Char (ord, chr)


encode :: String -> [(Int, Char)]
encode [] = []
encode [x] =[(1, x)]
encode (x:xs) = helper 1 x xs

helper :: Int -> Char -> String -> [(Int, Char)]
helper n c [] = [(n,c)]
helper n c (x:xs)
  | c == x  = helper (n+1) c xs
  | otherwise = (n, c) : helper 1 x xs 


data Tree a = Leaf | Node a (Tree a) (Tree a)

size :: Tree a -> Int
size tree = case tree of
    Leaf       -> 0
    Node _ l r  -> 1 + size l + size r

depth :: Tree a -> Int
depth tree = case tree of
    Leaf       -> 0
    Node a l r  -> max (depth l) (depth r) + 1


shift :: Int -> Char -> Char
shift n c = chr (ord c + n)

encrypt :: Int -> String -> String
encrypt n [] = []
encrypt n (x:xs) = shift n x : encrypt n xs

encrypt' :: Int -> String -> String
encrypt' n = map(shift n)

decrypt :: Int -> String -> String
decrypt n [] = []
decrypt n (x:xs) = shift (-n) x : decrypt n xs

decrypt' :: Int -> String -> String 
decrypt' n = map(shift (-n)) 
