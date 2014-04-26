-- Generate-and-test paradigm

-- Apply the generate-and-test paradigm to construct all symmetric, completely
-- balanced binary trees with a given number of nodes.

-- Example in Haskell:

-- *Main> symCbalTrees 5
-- [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]



data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
                       
mirror :: Tree Char -> Tree Char -> Bool
mirror Empty Empty = True
mirror (Branch _ x y) (Branch _ z t) = (mirror x t) && (mirror y z)
mirror _ _ = False

symmetric :: Tree Char -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n
  | odd n = map (\(x,y) -> Branch 'x' x y) [(x,y) | x <- subTree, y <- subTree]
  | otherwise = (map (\(x,y) -> Branch 'x' x y) [(x,y) | x <- subTree, y <- subTree']) ++
                (map (\(x,y) -> Branch 'x' x y) [(x,y) | y <- subTree, x <- subTree'])
  where subTree = cbalTree ((n-1) `quot` 2)
        subTree' = cbalTree (n `quot` 2)

symCbalTrees n = filter symmetric (cbalTree n)