-- (**) Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method
-- directly. I.e. don't explicitly create the sublists containing the
-- duplicates, as in problem 9, but only count them. As in problem P11, simplify
-- the result list by replacing the singleton lists (1 X) by X. Example in
-- Haskell:

-- P13> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
import Data.List

data ListItem a = Single a
                | Multiple Int a
                deriving Show

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect xs = map encodeHelper (group xs)
  where encodeHelper g
          | length g == 1 = Single (head g)
          | otherwise     = Multiple (length g) (head g)
                            
                            