{- Recursive types in Haskell -}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

{- Binary tree is a leaf or an inner node of two children -}
data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving (Eq, Show)
 -- 't' shows that it stores a value & two trees'=
 -- doing (Eq, Show) is a good idea

-- In order conversion of the binary tree contents to a list
inorder Empty             = [] -- empty list if the tree is empty
inorder (Leaf a)          = [a] -- only one leaf in the tree
inorder (InnerNode a l r) = (inorder l) ++ (a : (inorder r)) -- call left, root, right in order recursively

-- insert an element in-order into a tree that is in "sorted" order
-- assume that the tree is already sorted
insertInOrder x Empty    = Leaf x -- enforcing that leaf is the type
insertInOrder x (Leaf a)
    | x < a     = InnerNode a (Leaf x) Empty -- create a new inner node
    | otherwise = InnerNode a Empty (Leaf x)
insertInOrder x (InnerNode a l r)
    | x < a     = InnerNode a (insertInOrder x l) r
    | otherwise = InnerNode a l (insertInOrder x r)

{- Create some more functions on trees
   1. "height" gives the height of the longest branch of the tree
   2. "insertBalanced" takes an element and a tree and addes the element to the shortest branch
      of the tree.
   3. "addToRight" takes an element and adds the element to the rightmost branch 
   4. "removeInOrder" takes an element and a tree and removes the element from the tree
-}

height Empty             = 0
height (Leaf _)          = 1
height (InnerNode _ l r) = max (height l) (height r) + 1

{- Other versions:
    height (InnerNode _ l r)
        | (height l) > (height r) = (height l) + 1
        | otherwise               = (height r) + 1

   To simplify (avoid computing twice):
    height (InnerNode _ l r)
        | h1 > hr   = 1 + hl
        | otherwise = 1 + hr
        where 
            h1 = height l
            hr = height r
-}

insertBalanced x Empty             = Leaf x
insertBalanced x (Leaf a)          = InnerNode a (Leaf x) Empty -- or insertInOrder x (Leaf a); no need for sorting
insertBalanced x (InnerNode a l r)
    | (height l) > (height r)      = InnerNode a l (insertBalanced x r)
    | otherwise                    = InnerNode a (insertBalanced x l) r

addToRight x Empty = Leaf x
addToRight x (Leaf a) = InnerNode a Empty (Leaf x)
addToRight x (InnerNode a l r) = InnerNode a l (addToRight x r) 

{- Higher-order functions
    Functions that use and manipulate other functions
    
    ex) map, apply, etc. 
-}

{- foldInOrder takes a function, an initial value, and a tree:
    It recursively applies the function to each value of the tree using the
    initial value as the right operand, and the next value as the left operand.
    The result is the right operand for the next application of the function
    with the next value. -}
foldInOrder f i Empty             = i
foldInOrder f i (Leaf a)          = f a i
foldInOrder f i (InnerNode a l r) = foldInOrder f (f a (foldInOrder f i l)) r -- have to apply f to l -> a -> r