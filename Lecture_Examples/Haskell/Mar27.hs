{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}

-- write merge that takes 2 lists of numbers in order and returns
-- a combined list of both inputs, in order
-- merge [3, 10, 20] [5, 5, 18] => [3, 5, 5, 10, 18, 20]
merge [] l = l
merge l [] = l
merge (h1:t1) (h2:t2)
    | h1 < h2   = h1 : merge t1 (h2:t2)
    | otherwise = h2 : merge (h1:t1) t2


-- reverse a list
myreverse1 []   = []
myreverse1 (h:t) = (myreverse1 t) ++ [h]

-- reverse using function composition
myreverse2 []  = []
myreverse2 (h:t) = ((++) . myreverse2) t [h]


-- write a function to give the length of a list, 
-- but use function composition to define the recursive call
mylength [] = 0
mylength (h:t) = ((+) 1 . mylength) t

{- continuation passing style in Haskell 
append_cps l1 l2 return = 
    if null l1 
        then 
            return l2
        else 
            append_cps (tail l1) l2 (\v -> return ((head l1):v)) -- input return function: (\v -> v)
-}
-- another way to do continuation passing style
append_cps :: [a] -> [a] -> ([a] -> [a]) -> [a] -- specifying types
append_cps [] l return = return l
append_cps (h:t) l return = append_cps t l (\v -> return (h:v))


-- split a list into two lists
-- example input) split_cps [1, 2, 3, 4] (\l1 l2 -> [l1, l2])
split_cps [] return = return [] []
split_cps (h:t) return = split_cps t (\l1 l2 -> return (h:l2) l1) -- rotating around


-- write merge in cps
-- merge [3, 10, 20] [5, 5, 18] => [3, 5, 5, 10, 18, 20]
merge_cps [] l return = return l
merge_cps l [] return = return l
merge_cps (h1:t1) (h2:t2) return
    | h1 < h2   = merge_cps t1 (h2:t2) (\v -> return (h1:v))
    | otherwise = merge_cps (h1:t1) t2 (\v -> return (h2:v))


-- write mergesort in cps (split, sort each half, merge back) 
