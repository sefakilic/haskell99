-- A list of prime numbers. 

-- Given a range of integers by its lower and upper limit, construct a list of
-- all prime numbers in that range.

-- Example in Haskell:

-- P29> primesR 10 20
-- [11,13,17,19]

isPrime 2 = True
isPrime n = and [n `mod` x /= 0 | x <- 2:[3,5..(truncate (sqrt (fromIntegral n)))]]

primesR a b = filter isPrime [a..b]