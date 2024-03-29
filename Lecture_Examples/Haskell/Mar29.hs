{- Types in Haskell -}


{- To create type alias, use the type keyword.
   Scalar is an alias for type double -}
type Scalar = Double

doubleScale:: Scalar -> Scalar -- "::" defines the type
doubleScale x = 2 * x
{- Check type with ":type" -}


{- To create a new type, use the data keyword.
   Coordinate is the name of my new type.
   Coord2D and Coord3D are constructors for the type. -}
data Coordinate = Coord2D Double Double | Coord3D Double Double Double deriving (Show, Eq) 
{- can print/output Coordinate class by deriving (Show).
   For making "==" work for Coordinate types, derive (Eq). -}
{- Coord2D and Coord3D are actually functions that outputs Coordinate type -}


-- distance gives the distance between two coordinates
distance (Coord2D x y) (Coord2D a b) = sqrt((x - a) * (x - a) + (y - b) * (y - b))
