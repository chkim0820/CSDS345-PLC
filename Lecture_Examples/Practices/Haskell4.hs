{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import GHC.Num (Integer(IN))
import Data.Binary (Binary)
{-# HLINT ignore "Redundant bracket" #-}
{- binary tree that stores values; left, empty, or a node with two chilren -}
data BinaryTree t = Empty | Leaf t | InnerNode t (BinaryTree t) (BinaryTree t) deriving (Show, Eq)

{- inorder converts a tree to a list "in order"
    - left child -> node -> right child
-}
inorder Empty             = []
inorder (Leaf a)          = [a]
inorder (InnerNode a l r) = (inorder l) ++ a : inorder r

{- insert a new element in order into a tree
    - assume the tree is already in order
    - a node element is not smaller than its left children nor larger than its right
 -}
insert e Empty = Leaf e
insert e (Leaf a)
    | e < a     = InnerNode a (Leaf e) Empty
    | otherwise = InnerNode a Empty (Leaf e)
insert e (InnerNode a l r)
    | e < a     = InnerNode a (insert e l) r
    | otherwise = InnerNode a l (insert e r)

{- introduction to higher order functions -}

{- foldinorder takes a binary function, a value, and a tree.
        It traverses the tree in order; at each value, it applies the function with the value
        to the value in the tree, the result is the value applied.
        We will use node value as the left operand and the other value as the right operand 
        
        ex) foldinorder with + and initial value 0 => will sum all the values in the tree
-}
foldinorder f v Empty = v
foldinorder f v (Leaf a) = f a v
foldinorder f v (InnerNode a l r) = foldinorder f (f a (foldinorder f v l)) r

{-PRACTICES-}
-- height
height Empty = 0
height (Leaf a) = 1
height (InnerNode a l r)
    | (height l) > (height r) = 1 + (height l)
    | otherwise               = 1 + (height r)

-- addToRight
addToRight v Empty = Leaf v
addToRight v (Leaf a) = InnerNode a Empty (Leaf v)
addToRight v (InnerNode a l r) = InnerNode a l (addToRight v r)

-- addBalanced
addBalanced x Empty = Leaf x
addBalanced x (Leaf a) = InnerNode a (Leaf x) Empty
addBalanced x (InnerNode a l r)
    | (height l) > (height r) = InnerNode a l (addBalanced x r)
    | otherwise               = InnerNode a (addBalanced x l) r

-- applytotree
applytotree :: (t1 -> t2) -> BinaryTree t -> BinaryTree t
applytotree f Empty = Empty
applytotree f (Leaf a) = (Leaf (f a))
applytotree f (InnerNode a l r) = InnerNode (applytotree f a) (applytotree f l) (applytotree f r)