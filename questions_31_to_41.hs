import Data.List

-- question 31

isPrime n = and [n `mod` x /= 0 | x <- [2..n-1]]

-- question 32

myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- question 33

coprime x y = myGCD x y == 1

-- question 34

totient m = length [x | x <- [1..(m-1)], coprime x m]

-- question 35

primeFactors 1 = []
primeFactors n = fac : (primeFactors (n `quot` fac))
  where fac = (head [x | x <- [2..], n `mod` x == 0])
        
-- question 36        

prime_factors_mult n = map (\x -> (head x, length x)) (group (primeFactors n))

-- question 37

phi m = foldl (\acc (p,m) -> acc * (p-1) * p ^ (m-1)) 1 (prime_factors_mult m)

-- question 38
-- no solution required

-- question 39

primesR a b = filter isPrime [a..b]

-- question 40
goldbach m = head [(x, m-x) | x <- [2..m], isPrime x, isPrime (m-x)]

-- question 41

incIfOdd n = if odd n then n+1 else n
goldbachList m n = [goldbach x | let st = incIfOdd m, x <- [st, st+2 .. n]]

goldbachList' m n th = filter (\(pa,pb) -> pa>th && pb>th) (goldbachList m n)