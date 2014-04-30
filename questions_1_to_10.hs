-- question 1

myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- question 2

myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

-- question 3

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- question 4

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

-- question 5

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- question 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- question 7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- question 8

compress :: Eq a => [a] -> [a]
compress (x:[]) = [x]
compress (x:x':xs) 
  | x == x'   = compress (x':xs)
  | otherwise = x : compress (x':xs)

compress' :: Eq a => [a] -> [a]
compress' [x] = [x]
compress' (x:x':xs) = case x == x' of
  True -> compress' (x':xs)
  False -> x : compress' (x':xs)

-- question 9

pack :: Eq a => [a] -> [[a]]
pack (x:[]) = [[x]]
pack (x:x':xs)
  | x == x'   = (x:(head packed)) : (tail packed)
  | otherwise = [x]:packed
  where packed = pack (x':xs)

-- question 10
        
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)
