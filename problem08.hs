-- (**) Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.

-- Example in Haskell:

-- > compress "aaaabccaadeeee"
-- "abcade"

compress :: Eq a => [a] -> [a]
compress (x:[]) = [x]
compress (x:x':xs) 
  | x == x'   = compress (x':xs)
  | otherwise = x : compress (x':xs)

compress' :: Eq a => [a] -> [a]
compress' [x] = [x]
compress' (x:x':xs) = case x == x' of
  True -> compress' (x':xs)
  False -> x : compress' (x':xs)
