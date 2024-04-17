{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

{- Monads: a type where a value is wrapped in a "context" to give info about the value.
        ex) "promises" in web coding, Java's Optional -}
data RedBlack t = Red t | Black t

{- a Value that can either be a valid value or not a valid value -}
data Value t = Value t | NoValue deriving (Show)

{- we need two basic functions for the monad: return and bind -}
myreturn :: t -> Value t
myreturn x = Value x

-- does not require the resulting value to be of the same type as the input
mybind :: Value t -> (t -> Value s) -> Value s
mybind (Value x) f = f x
mybind NoValue _   = NoValue

{- Let's create some math operations on the monad -}
-- +++ is addition when adding values wrapped in a monad
(+++) vx vy = mybind vx (\x -> mybind vy (\y -> myreturn (x + y)))
(~~) vx vy = mybind vx (\x -> mybind vy (\y -> myreturn (x - y)))
(//) vx vy = mybind vx (\x -> mybind vy (\y -> if y == 0 then NoValue else myreturn (x/y)))

{- Haskell has some built-in monads: Maybe, IO, list

        data Maybe t = Just t | Nothing -- similar to above examples
        
        return function is "return"
        the bind function is ">>="
-}

(++++) mx my = mx >>= (\x -> my >>= (\y -> return (x + y)))
-- Better syntax for above; "do" syntax for doing the bind and return functions:
(~~~) mx my = do
    x <- mx -- shortcut
    y <- my
    return (x - y)
(///) mx my = do
    x <- mx
    y <- my
    if y == 0 then Nothing else return (x / y)

{-PRACTICES-}
-- vsqrt with Value
vsqrt x = mybind x (\v -> if v < 0 then NoValue else myreturn (sqrt v))
