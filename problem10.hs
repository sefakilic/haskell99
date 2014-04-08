-- (*) Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.

-- Example in Haskell:

-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

-- From P09
pack :: Eq a => [a] -> [[a]]
pack (x:[]) = [[x]]
pack (x:x':xs)
  | x == x'   = (x:(head packed)) : (tail packed)
  | otherwise = [x]:packed
  where packed = pack (x':xs)
        
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)