-- Compare the two methods of calculating Euler's totient function.

-- Use the solutions of problems 34 and 37 to compare the algorithms. Take the
-- number of reductions as a measure for efficiency. Try to calculate phi(10090)
-- as an example.

import Data.List

-- problem 34
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

coprime x y = myGCD x y == 1

totient m = length [x | x <- [1..(m-1)], coprime x m]

-- problem 37
primeFactors 1 = []
primeFactors n = fac : (primeFactors (n `quot` fac))
  where fac = (head [x | x <- [2..], n `mod` x == 0])
        
prime_factors_mult n = map (\x -> (head x, length x)) (group (primeFactors n))

phi m = foldl (\acc (p,m) -> acc * (p-1) * p ^ (m-1)) 1 (prime_factors_mult m)

