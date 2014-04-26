-- Binary search trees (dictionaries)

-- Use the predicate add/3, developed in chapter 4 of the course, to write a
-- predicate to construct a binary search tree from a list of integer numbers.

-- Then use this predicate to test the solution of the problem P56. 

-- *Main> construct [3, 2, 5, 7, 1]
-- Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
-- *Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
-- True
-- *Main> symmetric . construct $ [3, 2, 5, 7, 1]
-- True

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

add :: Ord a => Tree a -> a -> Tree a
add Empty m         = Branch m Empty Empty
add (Branch p l r) m
  | m < p     = Branch p (add l m) r
  | otherwise = Branch p l (add r m)
                
construct xs = foldl add Empty xs

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ x y) (Branch _ z t) = (mirror x t) && (mirror y z)
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right
