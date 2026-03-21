

isPrime :: Int -> Bool
isPrime n = null [x| x <- [2..n-1], mod n x == 0]

primes :: [Int]
primes = [x| x <- [1..], isPrime x]
