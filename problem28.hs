-- Sorting a list of lists according to length of sublists

-- a) We suppose that a list contains elements that are lists themselves. The
-- objective is to sort the elements of this list according to their
-- length. E.g. short lists first, longer lists later, or vice versa.

-- Example in Haskell:

-- Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- Prelude>["o","de","de","mn","abc","fgh","ijkl"]

import Data.List

lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs ys -> compare (length xs) (length ys))

-- b) Again, we suppose that a list contains elements that are lists
-- themselves. But this time the objective is to sort the elements of this list
-- according to their length frequency; i.e., in the default, where sorting is done
-- ascendingly, lists with rare lengths are placed first, others with a more
-- frequent length come later.

-- Example in Haskell:

-- lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

lfsort :: [[a]] -> [[a]]
lfsort ls = sortBy (\xs ys -> compare (freq xs) (freq ys)) ls
  where freq list = length (filter (\x -> x == length list) (map length ls))
