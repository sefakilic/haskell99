-- (**) Drop every N'th element from a list. 

-- Example in Haskell:

-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n-1) h) ++ (dropEvery t n)
  where (h,t) = splitAt n xs