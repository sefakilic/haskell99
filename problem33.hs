-- Determine whether two positive integer numbers are coprime. Two numbers are
-- coprime if their greatest common divisor equals 1.

-- * coprime 35 64
-- True

myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

coprime x y = myGCD x y == 1