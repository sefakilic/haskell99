-- (*) Remove the K'th element from a list. 
-- Example in Haskell:

-- *Main> removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
  | n <= 0        = error "invalid k"
  | n > length xs = error "invalid k"
  | otherwise     = (xs!!(n-1), (take (n-1) xs) ++ (drop n xs))
