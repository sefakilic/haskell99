import System.Random
import Data.List

-- question 21

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)

-- question 22

range :: Int -> Int -> [Int]
range a b = [a..b]

range' :: Int -> Int -> [Int]
range' a b 
  | a == b = [a]
  | otherwise = a : (range (a+1) b)
                
-- question 23

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  gen <- newStdGen
  return (map (\i -> xs!!(i-1)) (take n $ randomRs (1, length xs) gen))
  
-- question 24

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
  gen <- newStdGen
  return $ take n $ nub $ randomRs (1,m) gen
  
-- question 25

rnd_permu :: Eq a => [a] -> IO [a]
rnd_permu xs = do
  gen <- newStdGen
  return $ (take l $ nub $ map (\x -> xs !! (x-1)) (randomRs (1,l) gen))
  where l = length xs
        
-- question 26

combinations :: Int -> [a] -> [[a]]
combinations n all@(x:xs)
  | n > length all = []
  | n == 1         = [[x] | x <- all]
  | otherwise     = map (\acc -> x:acc) accs ++ combinations n xs
  where accs = combinations (n-1) xs

-- question 27

-- assuming the list does not contain repeating elements
group' :: [Int] -> [String] -> [[[String]]]
group' (n:[]) xs 
  | n /= length xs = error "group size error"
  | otherwise = [[xs]]
group' ns xs = let n = head ns
                   firstPartitions = combinations n xs
               in [fp:rps | fp <- firstPartitions, rps<-group' (tail ns) (xs\\fp)]
                  
-- question 28                  

lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs ys -> compare (length xs) (length ys))

lfsort :: [[a]] -> [[a]]
lfsort ls = sortBy (\xs ys -> compare (freq xs) (freq ys)) ls
  where freq list = length (filter (\x -> x == length list) (map length ls))
        

                  
