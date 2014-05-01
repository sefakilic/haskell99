-- question 54a

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
                       
-- question 55                       

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n
  | odd n = map (\(x,y) -> Branch 'x' x y) [(x,y) | x <- subTree, y <- subTree]
  | otherwise = (map (\(x,y) -> Branch 'x' x y) [(x,y) | x<-subTree, y<-subTree'])
                ++ 
                (map (\(x,y) -> Branch 'x' x y) [(x,y) | y<-subTree, x<-subTree'])
  where subTree = cbalTree ((n-1) `quot` 2)
        subTree' = cbalTree (n `quot` 2)
        
-- question 56

mirror :: Tree Char -> Tree Char -> Bool
mirror Empty Empty = True
mirror (Branch _ x y) (Branch _ z t) = (mirror x t) && (mirror y z)
mirror _ _ = False

symmetric :: Tree Char -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right

-- question 57

add :: Ord a => Tree a -> a -> Tree a
add Empty m         = Branch m Empty Empty
add (Branch p l r) m
  | m < p     = Branch p (add l m) r
  | otherwise = Branch p l (add r m)
                
construct xs = foldl add Empty xs

-- question 58

symCbalTrees n = filter symmetric (cbalTree n)

-- question 59



hbalTree :: Char -> Int -> [Tree Char]
hbalTree c 0 = [Empty]
hbalTree c 1 = [Branch c Empty Empty]
hbalTree c n = map (\(left,right) -> Branch c left right) all
  where all = (zip sub sub) ++ (zip sub sub') ++ (zip sub' sub')
        sub = hbalTree c (n-1)
        sub' = hbalTree c (n-2)
        
-- question 60
        
height :: Tree a -> Int
height Empty = 0
height (Branch _ left right) = 1 + max (height left) (height right)

isHeightBalanced :: Tree a -> Bool
isHeightBalanced Empty = True
isHeightBalanced (Branch _ left right) = abs (hl-hr) <= 1
  where hl = height left
        hr = height right

hbalTreeNodes :: Char -> Int -> [Tree Char]
hbalTreeNodes c 0 = [Empty]
hbalTreeNodes c 1 = [Branch c Empty Empty]
hbalTreeNodes c n = filter isHeightBalanced allTrees
  where allTrees = [Branch c left right | ln <- [0..(n-1)],
                    left <- precomputed !! ln,
                    right <- precomputed !! (n-1-ln)]
          where precomputed = map (hbalTreeNodes c) [0..(n-1)]
