-- Truth tables for logical expressions (2).

-- Continue problem P46 by defining and/2, or/2, etc as being operators. This
-- allows to write the logical expression in the more natural way, as in the
-- example: A and (A or not B). Define operator precedence as usual; i.e. as in
-- Java.

-- > table2 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False

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


table2 pred = putStrLn ("True True " ++ show (pred True True) ++ "\n" ++
                        "True False " ++ show (pred True False) ++ "\n" ++
                        "False True " ++ show (pred False True) ++ "\n" ++
                        "False False " ++ show (pred False False) )