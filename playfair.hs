import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import Data.Char (toUpper)
import System.Environment (getArgs)

countChar :: Char -> String -> Int
countChar c (x:rest)
  |  c == x    = 1 + countChar c rest
  | otherwise  = 0 + countChar x rest

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

countWeak :: [Int] -> Int
countWeak (x:xs)
  | x < 20    = 1 + countWeak xs
  | otherwise = 0 + countWeak xs
countWeak [] = 0

getFirstWord :: String -> String
getFirstWord (x:xs)
  | x == ' '  = []
  | otherwise = x : getFirstWord xs
getFirstWord [] = []

alphabet :: String
alphabet = filter (/= 'W')['A'..'Z']

formatKey :: String -> String
formatKey (x:xs)
  | x == 'W'  = 'V': formatKey xs 
  | otherwise = toUpper x : formatKey xs
formatKey [] = []

buildGrid :: String -> String
buildGrid key = nub(formatKey key ++ alphabet)

prepareText :: String -> String 
prepareText (a:b:rest)
  | a == b    = a : 'X' : prepareText(b:rest)
  | otherwise = a : b : prepareText rest
prepareText [a] = [a, 'X']
prepareText [] = []

toPairs :: String -> [(Char, Char)]
toPairs (a:b:rest) = (a, b) : toPairs rest

toPairs _ = []

data Mode = Encrypt | Decrypt

processPair :: Mode -> String -> (Char, Char) -> (Char, Char)
processPair mode grid (a, b) =
  let 
      indexA = fromJust (elemIndex a grid)
      indexB = fromJust (elemIndex b grid)

      rowA = indexA `div` 5
      colA = indexA `mod` 5
      rowB = indexB `div` 5
      colB = indexB `mod` 5
  in
      applyRules rowA colA rowB colB

  where
    
    shift = case mode of
            Encrypt -> 1
            Decrypt -> -1
    applyRules rA cA rB cB
      | rA == rB =
          let newColA = (cA + shift) `mod` 5
              newColB = (cB + shift) `mod` 5
          in (getCharByCoords rA newColA, getCharByCoords rB newColB)
      | cA == cB =
          let newRowA = (rA + shift ) `mod` 5
              newRowB = (rB + shift ) `mod` 5
          in (getCharByCoords newRowA cA, getCharByCoords newRowB cB)

      | otherwise = (getCharByCoords rA cB, getCharByCoords rB cA)
    
    getCharByCoords r c = grid !! (r * 5 + c)

pairsToString :: [(Char, Char)] -> String
pairsToString [] = []
pairsToString ((a, b):rest) = a : b : pairsToString rest

cryptMessage :: Mode -> String -> String -> String
cryptMessage mode key message =
  let 
      grid = buildGrid key

      cleanText = prepareText (formatKey message)

      pairs = toPairs cleanText

      processPairs = map (processPair mode grid) pairs

      result = pairsToString processPairs
  in result


main :: IO ()
main = do
    args <- getArgs

    if length args < 3
      then putStrLn "Using: playfair <encrypt|decrypt> <KEY> <MESSAGE>"
    else do
      let modeStr = args !! 0
      let key = args !! 1
      let msg = args !! 2

      let mode = if modeStr == "encrypt" then Encrypt else Decrypt

      let result = cryptMessage mode key msg

      putStrLn result


