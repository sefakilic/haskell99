-- Truth tables for logical expressions (3).

-- Generalize problem P47 in such a way that the logical expression may contain
-- any number of logical variables. Define table/2 in a way that
-- table(List,Expr) prints the truth table for the expression Expr, which
-- contains the logical variables enumerated in List.

-- > tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- -- infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True
 
-- -- infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False

import Data.List

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

prod 1 = [[True], [False]]
prod n = (map (\x -> True:x) rest) ++ (map (\x -> False:x) rest) 
  where  rest = prod (n-1)
  
tablen n pred = putStrLn $ myShow ((map (\vars -> (vars ++ [pred vars])) (prod n)))
  where myShow xs = intercalate "\n" (map (\row -> intercalate " " (map show row)) xs)
