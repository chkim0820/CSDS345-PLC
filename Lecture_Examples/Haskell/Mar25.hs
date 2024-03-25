{- this is a multi-line comment:
    we are going to introduce Haskell and the different ways to define a function
    -}


-- a "traditional" way to write a Haskell function
myappend l1 l2 =
    if l1 == []
        then l2
    else
        (head l1) : myappend (tail l1) l2