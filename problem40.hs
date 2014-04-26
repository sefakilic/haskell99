-- Goldbach's conjecture.

-- Goldbach's conjecture says that every positive even number greater than 2 is
-- the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
-- famous facts in number theory that has not been proved to be correct in the
-- general case. It has been numerically confirmed up to very large numbers
-- (much larger than we can go with our Prolog system). Write a predicate to
-- find the two prime numbers that sum up to a given even integer.

-- *goldbach 28
-- (5, 23)

primes = sieve [2..] where
  sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
  
isPrime 2 = True
isPrime n = and [n `mod` x /= 0 | x <- 2:[3,5..(n-1)]]

goldbach m = head [(x, m-x) | x <- [2..m], isPrime x, isPrime (m-x)]