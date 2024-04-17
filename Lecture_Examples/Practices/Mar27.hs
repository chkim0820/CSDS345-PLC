{- cps in haskell -}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use null" #-}

{- append in cps style using "normal" function coding -}
append_cps :: Eq a => [a] -> [a] -> ([a] -> [a]) -> [a]  -- explicitly types the output to be a list
append_cps l1 l2 return =
    if l1 == []
        then
            return l2
        else
            append_cps (tail l1) l2 (\v -> return ((head l1) : v))
            

{- append_cps using pattern matching -}
append_cps2 [] l2 return    = return l2
append_cps2 (h:t) l2 return = append_cps2 t l2 (\v -> return (h : v))

{- split in cps style
        - Takes a list & breaks into two lists with the odd indices in one & even in the other
 -}
split_cps [] return       = return [] []
split_cps (h:t) return    = split_cps t (\v1 v2 -> return (h:v2) v1)

{- PRACTICES -}
-- merge_cps with guards
merge_cps l [] return = return l
merge_cps [] l return = return l
merge_cps (h1:t1) (h2:t2) return
    | h1 < h2   = merge_cps t1 (h2:t2) (\v -> return (h1 : v))
    | otherwise = merge_cps (h1:t1) t2 (\v -> return (h2 : v))

-- mergesort_cps (split, sort each half, merge back)
mergesort_cps [] return    = return []
mergesort_cps l return = split_cps l (\v1 v2 -> return (merge_cps v1 v2 (\v -> )))