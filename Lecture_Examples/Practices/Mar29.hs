{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore default #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}

{- Types in haskell -}

{- you create a type alias with "type" -}
type Scale = Double

doubleScale:: Scale -> Scale
doubleScale n = 2*n
-- The same as Double -> Double since aliases; equivalent

{- you create a data type with "data"
   Coordinate is the name of the type
   Coord2D and Coord3D are constructors for the type; use either for Constructor

   Deriving states what type classes this type is a member of
        - "Show" for displaying values
 -}
--data Coordinate = Coord2D Double Double | Coord3D Double Double Double deriving (Show, Eq) 
data Coordinate t = Zero | Coord1D t | Coord2D t t | Coord3D t t t deriving (Show)
-- Coordinate can be of any type t; not just double like above

-- place Coordinate in the Eq class with a customize == function
instance (Floating t, Eq t) => Eq (Coordinate t) where -- instane of Eq on Coordinate t
    c1 == c2 = distance c1 c2 == 0 -- overwrote Eq so that equal when dist. is 0

-- Defining the distance functions using different types of Coordinates
--distance (Coord2D a b) (Coord2D c d) = sqrt ((a - c) * (a - c) + (b - d) * (b - d))

{- Helper functions to get the different values from a coordinate -}
getx Zero            = 0
getx (Coord1D a)     = a
getx (Coord2D a b)   = a
getx (Coord3D a b c) = a

gety Zero            = 0
gety (Coord1D a)     = 0
gety (Coord2D a b)   = b
gety (Coord3D a b c) = b

getz Zero            = 0
getz (Coord1D a)     = 0
getz (Coord2D a b)   = 0
getz (Coord3D a b c) = c

{- Compute the distance between any two coordinates -}
squared_diff a b dim = ((dim a - dim b) * (dim a - dim b))
distance a b = sqrt((squared_diff a b getx) + (squared_diff a b gety) + (squared_diff a b getz))

{- Cretae infix functions -}
(##) a b = distance a b -- use ## as the distance function

{- PRACTICES -}

-- operator that adds two coordinates
(-|-) (Coord3D x y z) a = Coord3D (x + getx a) (y + gety a) (z + getz a)
(-|-) a (Coord3D x y z) = Coord3D (x + getx a) (y + gety a) (z + getz a)
(-|-) (Coord2D x y) a   = Coord2D (x + getx a) (y + gety a)
(-|-) a (Coord2D x y)   = Coord2D (x + getx a) (y + gety a)
(-|-) (Coord1D x) a     = Coord1D (x + getx a)
(-|-) a (Coord1D x)     = Coord1D (x + getx a)
(-|-) Zero Zero         = Zero

-- overriding + operator 
