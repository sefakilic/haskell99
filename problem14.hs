-- (*) Duplicate the elements of a list. 

-- > dupli [1, 2, 3]
-- [1,1,2,2,3,3]

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)