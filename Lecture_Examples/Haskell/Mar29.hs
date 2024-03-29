{- Types in Haskell -}


{- To create type alias, use the type keyword.
   Scalar is an alias for type double -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
type Scalar = Double

doubleScale:: Scalar -> Scalar -- "::" defines the type
doubleScale x = 2 * x
{- Check type with ":type" -}


{- To create a new type, use the data keyword.
   Coordinate is the name of my new type.
   Coord2D and Coord3D are constructors for the type. -}
--data Coordinate = Coord2D Double Double | Coord3D Double Double Double deriving (Show, Eq) 
{- can print/output Coordinate class by deriving (Show).
   For making "==" work for Coordinate types, derive (Eq). -}
{- Coord2D and Coord3D are actually functions that outputs Coordinate type -}


{- make coordinate be a parameterized type -}
data Coordinate t = Zero | Coord1D t | Coord2D t t | Coord3D t t t deriving (Show)

-- Overriding Eq operator "=="
-- Specified type Floating, Eq
instance (Floating t, Eq t) => Eq (Coordinate t) where
    a == b = (distance a b) == 0


-- let's create "getter" functions for the coordinates
getx Zero            = 0
getx (Coord1D x)     = x
getx (Coord2D x y)   = x
getx (Coord3D x y z) = x

gety Zero            = 0
gety (Coord1D x)     = 0
gety (Coord2D x y)   = y
gety (Coord3D x y z) = y

getz Zero            = 0
getz (Coord1D x)     = 0
getz (Coord2D x y)   = 0
getz (Coord3D x y z) = z

-- distance gives the distance between two coordinates
-- Helper functions:
squarediff a b dim = (dim a - dim b) * (dim a - dim b)
-- Below uses the above getter methods
distance a b = sqrt(squarediff a b getx + squarediff a b gety + squarediff a b getz)

{-  
    First ver:
    distance (Coord2D x y) (Coord2D a b) = sqrt((x - a) * (x - a) + (y - b) * (y - b)) 
    Second ver:
    distance a b = sqrt((getx a - getx b) * (getx a - getx b) + (gety a - gety b) * (gety a - gety b))
-}


{- Create an operator that does the distance between two coordinates 
     x ## y => returns distance between x and y -}
(##) a b = distance a b
-- Equivalent ver: (##) = distance


{- Create a new operator to add two coordinate,
   (a -|- b) or (a &+ b)
   The resulting coordinate should be the "wider" of the operands.
    Zero -|- Coord3D 4 5 6 => Coord3D 4 5 6 -}
(-|-) (Coord3D x y z) a = Coord3D (x + getx a) (y + gety a) (z + getz a)
(-|-) a (Coord3D x y z) = Coord3D (x + getx a) (y + gety a) (z + getz a)
(-|-) (Coord2D x y) a   = Coord2D (x + getx a) (y + gety a)
(-|-) a (Coord2D x y)   = Coord2D (x + getx a) (y + gety a)
(-|-) (Coord1D x) a     = Coord1D (x + getx a)
(-|-) a (Coord1D x)     = Coord1D (x + getx a)
(-|-) Zero Zero         = Zero