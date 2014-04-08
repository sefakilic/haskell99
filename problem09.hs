-- (**) Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.

-- Example in Haskell:

-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
--              'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: Eq a => [a] -> [[a]]
pack (x:[]) = [[x]]
pack (x:x':xs)
  | x == x'   = (x:(head packed)) : (tail packed)
  | otherwise = [x]:packed
  where packed = pack (x':xs)
