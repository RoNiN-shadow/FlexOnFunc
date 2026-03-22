
doubleme x= x + x

doubleUs x y = doubleme x + doubleme y 

doubleSmallNumber x = if x <= 100
                    then x*2
                    else x

doubleSmallNumber' x = (if x <= 100 then x*2 else x) + 1

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

head' :: [a] -> a
head' [] = error "Can't tell head on an empty list, damn!"
head'(x:_) = x 

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x,y] = "The list has only two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long, here are first two elements: " ++ show x ++ " and " ++ show y
