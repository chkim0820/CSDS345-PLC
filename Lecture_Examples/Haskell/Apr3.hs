{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use camelCase" #-}

{- Haskell Monads -}

{- We will create a 'Value' monad that has a context to determine if the value is valid -}
data Value t = Value t | NoValue deriving (Show)

-- the return function wraps a value into our Value monad
myreturn:: t -> Value t
myreturn a = Value a

-- the bind function applies an arbitrary function to our monad value
mybind:: Value t -> (t -> Value s) -> Value s
mybind (Value a) f = f a
mybind NoValue _ = NoValue

-- create arithmetic functions that do the arithmetic on the monads (CPS)
(+++) va vb = mybind va (\a -> mybind vb (\b -> myreturn (a + b))) 
(~~) va vb  = mybind va (\a -> mybind vb (\b -> myreturn (a - b)))
(//) va vb  = mybind va (\a -> mybind vb (\b -> if b == 0 then NoValue else myreturn (a / b))) -- /0 gives NoValue

-- Create the function for square root: vsqrt
vsqrt va = mybind va (\a -> if a < 0 then NoValue else myreturn (sqrt a)) -- negative sqrt cannot happen

{- Haskell has built-in monads: Maybe, IO (Input & Output), list
   data Maybe t = Just t | Nothing
   
   The return function for monads is "return"
   The bind function is (>>=)
-}

-- Behaves exactly the same as our functions above; Haskell's monads are shortcuts
-- Below are all valid syntaxes:
(++++) ma mb = (>>=) ma (\a -> (>>=) mb (\b -> return (a + b)))
(~~~) ma mb  = ma >>= (\a -> mb >>= (\b -> return (a - b)))
(***) ma mb = do 
    a <- ma
    b <- mb
    return (a * b)
(///) ma mb = do
    a <- ma
    b <- mb
    if b == 0 then Nothing else return (a / b)

{- Practices:
    1. msqrt squareroot using the Maybe monad
    2. mapply takes 2 Maybes and an arbitrary function & applies the function to the Maybes.
            mapply (Just 3) (*) (Just 5) => (Just 15)
    3. in_order takes a list & returns the list wrapped in a Maybe if the list is in order and Nothing if not.
        Write it using everything in monads.
            in_order [1,2,3,4] => Just [1,2,3,4]
            in_order [1,2,4,3] => Nothing
-}

msqrt mx = do
    x <- mx
    if x < 0 then Nothing else return (sqrt x)

mapply ma f mb = do
    a <- ma
    b <- mb
    return (f a b)

in_order [] = []
in_order [a] = return [a]
in_order (h1:(h2:t))
    | h1 <= h2  = in_order (h2:t)
    | otherwise = False