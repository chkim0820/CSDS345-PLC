{- 
    Name: Chaehyeon Kim (cxk445)
    Description: For CSDS 345 Haskell practice assignment
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use camelCase" #-}

{- 1: rotate
        - Inputs: 3 elements & 1 list
        - Returns a list where each occurrence of the first element is replaced by the second, etc.
-}
rotate _ _ _ [] = []
rotate a b c (h:t)
    | a == h    = b : rotate a b c t
    | b == h    = c : rotate a b c t
    | c == h    = a : rotate a b c t
    | otherwise = h : rotate a b c t

{- 2: squareroot
        - Inputs: 2 numbers (value & iteration)
        - Returns the squareroot using Newton's iteration rounds
-}
squareroot val it = squareroot_cps val it (\v -> v)
-- Helper function for running squareroot in cps
squareroot_cps val 0 return = return val
squareroot_cps val it return = squareroot_cps val (it - 1) (\old -> return (old - ((old * old) - val) / (2 * old)))

{- 3: listmax
        - Inputs: 1 list (non-empty list of numbers)
        - Returns the maximum value in the list
-}