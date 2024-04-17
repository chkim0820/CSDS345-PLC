-- this is a single line comment
{- this is a multi-line comment -}

{- write append three ways; first the "normal" way -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import GHC.IO.Handle (hGetChar)
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-
    ghci> :type myappend1 -- can find the type by using ':type' command
    myappend1 :: Eq a => [a] -> [a] -> [a]
    Notes:
        - 'a' allows Eq since using l1 == []
        - Can interpret as 2 inputs and 1 input of the same time ([a] and [a] -> [a])
        - Or a single input that produces a function taking in a list & producing a list ([a] -> [a] and [a])
-}
myappend1 l1 l2 = 
    if l1 == [] -- if list 1 equals empty list
        then 
            l2 -- then return l2
        else
            -- (cons (car l1) (myappend (cdr l1 l2))) => in Scheme
            (head l1) :  myappend1 (tail l1) l2 -- () for grouping

{- append using a "lambda"; create functions separate from binding -}
myappend2 :: (Eq a) => [a] -> [a] -> [a] -- has to specify that a is Eq
myappend2 =
    \ l1 l2 ->
        if l1 == [] -- cannot infer type Eq with l1 == [] alone
            then 
                l2
            else 
                (head l1) : myappend2 (tail l1) l2

{- append using "pattern matching" -}
myappend3 [] l    = l
myappend3 (h:t) l = h : myappend3 t l


{- reverse a list -}
-- typematching
myreverse1 []    = []
myreverse1 (h:t) = (myreverse1 t) ++ [h] -- ++ is for append

-- function composition
myreverse2 []    = []
myreverse2 (h:t) = ((++) . myreverse2) t [h] -- append with myreverse

{- remove all copies of an element from a list -}
removeall a [] = []
removeall a (h:t) =
    if h == a
        then 
            removeall a t
        else
            h : removeall a t

{- removeall using pattern matching and a "guard" -}
removeall2 _ [] = []
removeall2 a (h:t) -- cannot do (a:t)
    | a == h    = removeall2 a t
    | otherwise = h : removeall2 a t



{- PRACTICES -}

-- square
-- normal if/then
squares1 l =
    if l == []
        then
            []
        else
            ((head l) * (head l)) : squares1 (tail l)
-- lambda function
squares2 :: (Eq a, Num a) => [a] -> [a]
squares2 =
    \l ->
        if l == []
            then
                []
            else
                ((head l) * (head l)) : squares2 (tail l)
-- pattern matching
squares3 [] = []
squares3 (h:t) = (h * h) : squares3 t

-- replaceall
replaceall3 _ _ [] = []
replaceall3 a b (h:t)
    | a == h    = b : replaceall3 a b t
    | otherwise = h : replaceall3 a b t

-- merge
merge [] l = l
merge l [] = l
merge (h1:t1) (h2:t2)
    | h1 < h2 = h1 : merge t1 (h2:t2)
    | otherwise = h2 : merge (h1:t1) t2

-- len
len []    = 0
len (h:t) = ((+) 1 . len) t