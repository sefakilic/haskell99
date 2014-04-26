-- Given a range of integers by its lower and upper limit, print a list of all
-- even numbers and their Goldbach composition.

-- In most cases, if an even number is written as the sum of two prime numbers,
-- one of them is very small. Very rarely, the primes are both bigger than say
-- 50. Try to find out how many such cases there are in the range 2..3000.

-- *Exercises> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- *Exercises> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]

isPrime 2 = True
isPrime n = and [n `mod` x /= 0 | x <- 2:[3,5..(n-1)]]

goldbach m = head [(x, m-x) | x <- [2..m], isPrime x, isPrime (m-x)]

incIfOdd n = if odd n then n+1 else n
goldbachList m n = [goldbach x | let st = incIfOdd m, x <- [st, st+2 .. n]]

goldbachList' m n th = filter (\(pa,pb) -> pa>th && pb>th) (goldbachList m n)