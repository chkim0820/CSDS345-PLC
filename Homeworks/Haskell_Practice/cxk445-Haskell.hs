{- 
    Name: Chaehyeon Kim (cxk445)
    Description: For CSDS 345 Haskell practice assignment
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use list literal" #-}

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






{- 5: Creating a type that allows us to have nested lists -}
-- Type ListElement has two possible constructors; Element and SubList
data ListElememt t = Element t | SubList [ListElememt t] deriving (Show, Eq)

-- Creating a function 'grotate' using ListElement type above
grotate :: Eq a => a -> a -> a -> [ListElememt a] -> [ListElememt a] -- setting type
grotate _ _ _ [] = []
grotate a b c (h:t)
    | (??) h                 = (makeSubList (grotate a b c (getList h))) : grotate a b c t
    | (getElement h) == a    = (makeElement b) : grotate a b c t
    | (getElement h) == b    = (makeElement c) : grotate a b c t
    | (getElement h) == c    = (makeElement a) : grotate a b c t
    | otherwise              = h : grotate a b c t
--isSubList [a] -> Bool -- Replace this with your actual check for SubList type

-- Below are operators to check if the input is SubList or not
(??) :: ListElememt t -> Bool
(??) (SubList a) = True
(??) (Element a) = False
-- returns the list inside the SubList object
getList :: ListElememt t -> [ListElememt t]
getList (SubList a) = a
-- returns the element of ListElement object
getElement :: ListElememt t -> t
getElement (Element a) = a
-- makes the input into a ListElement type
makeElement :: t -> ListElememt t
makeElement a = (Element a)
-- returns the SubList from a list
makeSubList :: [ListElememt t] -> ListElememt t
makeSubList a = (SubList a)