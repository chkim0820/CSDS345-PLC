{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Eta reduce" #-}

{- higher order functions: functions that manipulate other functions

    ex)
        - map: takes a function and a list and applies the function to each element of the list
        - filter: takes a function and a list and outputs a list of the elements that pass the filter function 
        - foldl: foldl (+) 0 [1 2 3] ; takes the previous result and moves on
        - foldr: foldr (+) 0 [1 2 3] ; opposite of above; (1 + (2 + (3 + 0)))
                                     ; another example: foldr (-) 0 [1, 2, 3, 4]
-}

-- get the first element of each sublist of a list (get the first column of a matrix)
firsts = map head

-- apply: takes a function and two lists & applies the function to the two lists elementwise
apply f [] [] = []
apply f (h1:t1) (h2:t2) = (f h1 h2) : apply f t1 t2

-- dotproduct: multiply the values of two vectors element-wise, sum the products
dotproduct v1 v2 = foldl (+) 0 (apply (*) v1 v2)

-- apply practice; takes a function and two lists & applies the function to the two lists elementwise
apply1 f [] [] = []
apply1 f (h1:t1) (h2:t2) = (f h1 h2) : apply f t1 t2
-- dotproduct practice; multiply the values of two vectors element-wise, sum the products
dotproduct1 :: Num t => [t] -> [t] -> t
dotproduct1 l1 l2 = foldl (+) 0 (apply (*) l1 l2) -- can't do . b/c input is a list not a function

-- return true if all the sublists of a list containing list are empty
allempty :: (Eq a) => [[a]] -> Bool
allempty = (foldl (&&) True) . (map (\x -> x == []))

-- allempty practice; True if a lits containing list are empty
allempty1 :: (Eq a) => [[a]] -> Bool
allempty1 = (foldl (&&) True) . (map (\l -> l == []))

-- split [1, 2, 3, 4, 5] => [[1,3,5], [2,4]]
split :: [a] -> [[a]]
split = foldr (\x pair -> [x : ((head . tail) pair), head pair]) [[], []] 

-- writing split again as a practice
split1 :: [a] -> [[a]]
split1 = foldr (\x pair -> [x : ((head . tail) pair), head pair]) [[], []]


-- more practice?

-- myreverse
myreverse :: [a] -> [a]
myreverse l = foldl (\acc x -> x : acc) [] l -- accumulator first...
