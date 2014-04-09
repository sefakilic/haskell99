-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.

-- Example in Haskell:

-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

-- From P09
pack :: Eq a => [a] -> [[a]]
pack (x:[]) = [[x]]
pack (x:x':xs)
  | x == x'   = (x:(head packed)) : (tail packed)
  | otherwise = [x]:packed
  where packed = pack (x':xs)
        
data ListItem a = Single a
                | Multiple Int a
                deriving Show

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = map f (pack xs)
  where f y
          | length y == 1 = Single (head y)
          | otherwise     = Multiple (length y) (head y)
