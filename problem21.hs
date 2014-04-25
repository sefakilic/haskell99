-- Insert an element at a given position into a list.
-- Example in Haskell:

-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)