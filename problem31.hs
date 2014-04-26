-- Determine whether a given integer number is prime. 

-- P31> isPrime 7
-- True

isPrime n = and [n `mod` x /= 0 | x <- [2..n-1]]

-- can be improved by checking until sq n, instead of n-1
