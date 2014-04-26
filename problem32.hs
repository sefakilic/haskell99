-- Determine the greatest common divisor of two positive integer numbers. Use
-- Euclid's algorithm.

-- [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]

myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)