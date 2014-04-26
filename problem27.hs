-- Group the elements of a set into disjoint subsets.

-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
-- 2, 3 and 4 persons? Write a function that generates all the possibilities and
-- returns them in a list.

-- You may find more about this combinatorial problem in a good book on discrete
-- mathematics under the term "multinomial coefficients".

-- Example in Haskell:

-- P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)
 
-- 27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)

import Data.List

combinations :: Int -> [a] -> [[a]]
combinations n all@(x:xs)
  | n > length all = []
  | n == 1         = [[x] | x <- all]
  | otherwise     = map (\acc -> x:acc) accs ++ combinations n xs
  where accs = combinations (n-1) xs
        
-- assuming the list does not contain repeating elements
group' :: [Int] -> [String] -> [[[String]]]
group' (n:[]) xs 
  | n /= length xs = error "group size error"
  | otherwise = [[xs]]
group' ns xs = let n = head ns
                   firstPartitions = combinations n xs
               in [fp:rps | fp <- firstPartitions, rps <- group' (tail ns) (xs \\ fp)]
                 

