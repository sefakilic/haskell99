import Data.List

-- question 11 

-- from question 9
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


-- question 12
                         
decodeModified :: [ListItem a] -> [a]
decodeModified xs = foldl (++) [] (map helper xs)
  where helper (Single c) = [c]
        helper (Multiple n c) = take n (repeat c)


-- question 13

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect xs = map encodeHelper (group xs)
  where encodeHelper g
          | length g == 1 = Single (head g)
          | otherwise     = Multiple (length g) (head g)
                            

-- question 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- question 15

repli :: [a] -> Int -> [a]
repli [] n = []
repli (x:xs) n = (take n (repeat x)) ++ repli xs n

-- question 16

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n-1) h) ++ (dropEvery t n)
  where (h,t) = splitAt n xs
        
-- question 17
        
split :: [a] -> Int -> ([a], [a])
split xs n = splitHelper ([], xs) n
  where splitHelper (a,b) 0 = (a,b)
        splitHelper (a,b) n = splitHelper (a ++ [head b], tail b) (n-1)
        
-- question 18

slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k-i+1) (drop (i-1) xs)

-- question 19

rotate :: [a] -> Int -> [a]
rotate xs n 
  | n == 0 = xs
  | n > 0  = rotate ((tail xs) ++ [head xs]) (n-1)
  | n < 0  = rotate ((last xs) : (init xs)) (n+1)
             
-- question 20

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
  | n <= 0        = error "invalid k"
  | n > length xs = error "invalid k"
  | otherwise     = (xs!!(n-1), (take (n-1) xs) ++ (drop n xs))
