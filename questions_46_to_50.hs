import Data.List

-- question 46

not' True = False
not' False = True

and' True True = True
and' _ _       = False

or' True True  = True
or' True False = True
or' False True = True
or' _ _        = False

nand' a b = not' (and' a b)

nor' a b = not' (or' a b)

xor' True False = True
xor' False True = True
xor' _ _        = False

impl' True False = False
impl' _ _        = True

equ' True True   = True
equ' False False = True
equ' _ _         = False

table pred = putStrLn ("True True " ++ show (pred True True) ++ "\n" ++
                       "True False " ++ show (pred True False) ++ "\n" ++
                       "False True " ++ show (pred False True) ++ "\n" ++
                       "False False " ++ show (pred False False) )
             
-- question 47

table2 pred = putStrLn ("True True " ++ show (pred True True) ++ "\n" ++
                        "True False " ++ show (pred True False) ++ "\n" ++
                        "False True " ++ show (pred False True) ++ "\n" ++
                        "False False " ++ show (pred False False) )
              
-- question 48

prod 1 = [[True], [False]]
prod n = (map (\x -> True:x) rest) ++ (map (\x -> False:x) rest) 
  where  rest = prod (n-1)
  
tablen n pred = putStrLn $ myShow ((map (\vars -> (vars ++ [pred vars])) (prod n)))
  where myShow xs = intercalate "\n" (map (\row -> intercalate " " (map show row)) xs)
        
-- question 49

-- from Wikipedia [http://en.wikipedia.org/wiki/Gray_code] 

-- The binary-reflected Gray code list for n bits can be generated recursively
-- from the list for n−1 bits by reflecting the list (i.e. listing the entries
-- in reverse order), concatenating the original list with the reversed list,
-- prefixing the entries in the original list with a binary 0, and then
-- prefixing the entries in the reflected list with a binary 1. For example,
-- generating the n = 3 list from the n = 2 list:

-- 2-bit list:	00, 01, 11, 10	 
-- Reflected:	 	10, 11, 01, 00
-- Prefix old entries with 0:	000, 001, 011, 010,	 
-- Prefix new entries with 1:	 	110, 111, 101, 100
-- Concatenated:	000, 001, 011, 010,	110, 111, 101, 100
        
gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = map ('0':) rec ++ map ('1':) (reverse rec)
  where rec = gray (n-1)
        
-- question 50
        
-- data Tree a = Empty | Branch a (Tree a) (Tree a)
-- huffman compression
        
-- proper implementation should use priority queues rather than sorting the list
-- again and again. 

data Tree a = LeafNode a | InternalNode a (Tree a) (Tree a) deriving (Show,Eq)

huffmanTree :: [Tree (Char,Int)] -> Tree (Char,Int)
huffmanTree (t:[]) = t
huffmanTree ts = huffmanTree $ (InternalNode ('*', x+y) a b) : rest
  where (a:b:rest) = sortBy (\t t' -> compare (getFreq t) (getFreq t')) ts
        x = getFreq a
        y = getFreq b
        getFreq (LeafNode (_,f)) = f
        getFreq (InternalNode (_,f) _ _) = f

huffmanTree2Code :: Tree (Char,Int) -> [(Char, String)]
huffmanTree2Code (LeafNode (c,_)) = [(c,"")]
huffmanTree2Code (InternalNode (c,_) l r) = map (\(c,s) -> (c,'0':s)) hl ++
                                            map (\(c,s) -> (c,'1':s)) hr
  where hl = huffmanTree2Code l
        hr = huffmanTree2Code r
                                            
huffman fs = huffmanTree2Code $ huffmanTree $ map (\(c,f) -> LeafNode (c,f)) fs


