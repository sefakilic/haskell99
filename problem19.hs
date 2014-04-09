-- (**) Rotate a list N places to the left.

-- Hint: Use the predefined functions length and (++). 

-- Examples in Haskell:

-- *Main> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
 
-- *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate xs n 
  | n == 0 = xs
  | n > 0  = rotate ((tail xs) ++ [head xs]) (n-1)
  | n < 0  = rotate ((last xs) : (init xs)) (n+1)