-- Calculate Euler's totient function phi(m). 

-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m.

-- Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1. 

-- * totient 10
-- 4

myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

coprime x y = myGCD x y == 1

totient m = length [x | x <- [1..(m-1)], coprime x m]