-- Determine the prime factors of a given positive integer. Construct a flat
-- list containing the prime factors in ascending order.

-- > primeFactors 315
-- [3, 3, 5, 7]

-- using Haskell's lazy-eval feature 
primeFactors 1 = []
primeFactors n = fac : (primeFactors (n `quot` fac))
  where fac = (head [x | x <- [2..], n `mod` x == 0])
        
