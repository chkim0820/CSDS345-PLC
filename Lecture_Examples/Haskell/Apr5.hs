{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Eta reduce" #-}

{-  Higher Order Functions 

    map: takes a function and a list and applies the function to each element of the list (map :: (a -> b) -> [a] -> [b])
        i.e. map length ["Hello", "345", "Class"] => [5,3,5]

    filter: takes a function and a list
        - makes a new list keeping the elements that the function returns True on (filter :: (a -> Bool) -> [a] -> [a])
        i.e. filter (> 0) [3,0,2,-6,8,-10] => [3,2,8]

    foldl: takes a function and an initial value & applies the function sequentially
           to the elements in the list from left to right (foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b)
        i.e. foldl (-) 0 [1,2,3,4,5] => -15

    foldr: takes a function and an initial value & applies the function sequentially
           to the elements in the list from right to left (foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b)
        i.e. foldr (-) 0 [1,2,3,4,5] => 3

-}

-- firsts gets a list containing the first elements of all sublists of a list
-- firsts [[1,2,3],[4,5,6]] => [1,4]
firsts = map head   -- applies the head function to each element of the list
                    -- head returns the first element of a list
                    -- currying; no input

-- allEmpty takes a list with sublists and returns True if every sublists is the empty list
allEmpty :: [[a]] -> Bool -- Takes a list of lists and returns boolean; can do currying
allEmpty = null . (filter (not . null))
{- Without currying:
     allEmpty l = null (filter (not . null) l)
 -}

-- Let's create a higher order function
-- Apply takes a function and two lists and outputs a list 
-- that is the result of applying the function to the elements of the lists pairwise
-- apply :: (a -> b -> c) -> [a] -> [b] -> [c]
apply _ [] [] = [] -- assume equal number of elements between the two input lists
apply f (h1:t1) (h2:t2) = (f h1 h2): apply f t1 t2

-- dotproduct of two vectors; [a,b,c] [d,e,f] = ad + be + cf
dotproduct v1 v2 = foldl (+) 0 (apply (*) v1 v2)

-- split [1,2,3,4,5] -> [[1,3,5],[2,4]]; a single value => foldl or foldr
-- Curried:
split :: [a] -> [[a]]
split = foldr (\x [a,b] -> [x:b,a]) [[],[]]
-- Original: split l = foldr (\x [a,b] -> [x:b,a]) [[],[]] l

-- myreverse
myreverse (h:t) = foldr (\x t -> (x:))

-- quicksort (all elements smaller than head in front, all elements larger in the back)
qsort [] = []
qsort [a] = [a]
qsort (h:t) = qsort (filter (< h) t) ++ [h] ++ qsort (filter (>= h) t)