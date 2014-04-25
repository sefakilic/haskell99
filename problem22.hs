-- Create a list containing all integers within a given range.

-- Example in Haskell:

-- Prelude> range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range a b = [a..b]

range' :: Int -> Int -> [Int]
range' a b 
  | a == b = [a]
  | otherwise = a : (range (a+1) b)
                
                