-- (**) replicate the elements of a list a given number of times. Example in
-- Haskell:

-- > repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n = (take n (repeat x)) ++ repli xs n