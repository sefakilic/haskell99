-- Determine the prime factors of a given positive integer.

-- Construct a list containing the prime factors and their multiplicity. 

-- *Main> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

import Data.List

primeFactors 1 = []
primeFactors n = fac : (primeFactors (n `quot` fac))
  where fac = (head [x | x <- [2..], n `mod` x == 0])
        
prime_factors_mult n = map (\x -> (head x, length x)) (group (primeFactors n))