-- (*) Split a list into two parts; the length of the first part is given.

-- Do not use any predefined predicates.

-- Example in Haskell:

-- *Main> split "abcdefghik" 3
-- ("abc", "defghik")

split :: [a] -> Int -> ([a], [a])
split xs n = splitHelper ([], xs) n
  where splitHelper (a,b) 0 = (a,b)
        splitHelper (a,b) n = splitHelper (a ++ [head b], tail b) (n-1)