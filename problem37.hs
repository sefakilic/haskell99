-- Calculate Euler's totient function phi(m) (improved).

-- See problem 34 for the definition of Euler's totient function. If the list of
-- the prime factors of a number m is known in the form of problem 36 then the
-- function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2
-- m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a
-- given number m. Then phi(m) can be calculated with the following formula:

-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
--          (p2 - 1) * p2 ** (m2 - 1) * 
--          (p3 - 1) * p3 ** (m3 - 1) * ...

-- Note that a ** b stands for the b'th power of a. 

import Data.List

primeFactors 1 = []
primeFactors n = fac : (primeFactors (n `quot` fac))
  where fac = (head [x | x <- [2..], n `mod` x == 0])
        
prime_factors_mult n = map (\x -> (head x, length x)) (group (primeFactors n))

phi m = foldl (\acc (p,m) -> acc * (p-1) * p ^ (m-1)) 1 (prime_factors_mult m)