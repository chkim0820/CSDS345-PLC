{-  this is a multi-line comment:
    input 'GHCI' to execute the file
    -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use foldr" #-}
{-# ANN module ("hlint: ignore Use <$>") #-}



-- a "traditional" way to write a Haskell function
myappend l1 l2 =
    if null l1    -- previously) l1 == []; valid as long as the types are equal
        then l2
    else
        (head l1) : myappend (tail l1) l2

-- coding a function as a "lambda" (\)
myappend2 :: (Eq a) => [a] -> [a] -> [a]  --can't do 'l1 == []' without this b/c loose typing w/ lambdas
myappend2 = 
    \l1 l2 -> 
        if l1 == []
            then
                l2
            else
                (head l1) : myappend2 (tail l1) l2


{-
myappend2 = 
    \l1 l2 -> 
        if null 11        --can't do 'l1 == []' b/c loose typing w/ lambdas
            then
                l2
            else
                (head l1) : myappend2 (tail l1) l2
                -}


-- defining a function using pattern matching
myappend3 [] l2 = l2
myappend3 (h:t) l2 = h : myappend3 t l2

{- write squares:
    squares [1,2,3] => [1,4,9]
    write squares using all 3 methods -}