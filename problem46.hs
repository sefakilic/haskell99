-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
-- logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and
-- B succeed.

-- A logical expression in two variables can then be written as in the following
-- example: and(or(A,B),nand(A,B)).

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

-- Now, write a predicate table/3 which prints the truth table of a given
-- logical expression in two variables.

-- > table (\a b -> (and' a (or' a b)))
-- True True True
-- True False True
-- False True False
-- False False False

table pred = putStrLn ("True True " ++ show (pred True True) ++ "\n" ++
                       "True False " ++ show (pred True False) ++ "\n" ++
                       "False True " ++ show (pred False True) ++ "\n" ++
                       "False False " ++ show (pred False False) )