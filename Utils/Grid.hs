-- |
-- Module      : Utils.Grid
-- Description : Contains methods to handle working with grids.
-- Copyright   : (c) Pieter De Clercq, 2022
-- License     : MIT
--
-- Contains methods to handle working with grids.
module Utils.Grid (module Utils.Grid) where

import Data.Set (Set)
import qualified Data.Set as Set

-- | A coordinate defined as a pair of (x, y)
type Coordinate = (Int, Int)

-- | A direction in a 2D plane.
data Direction = East | North | South | West deriving (Eq, Ord, Show)

-- | A 2d grid of elements.
type Grid a = [[a]]

-- | Calculates the Chebyshev distance between two coordinates.
chebyshev :: Coordinate -> Coordinate -> Int
chebyshev (x1, y1) (x2, y2) = maximum $ map abs [x1 - x2, y1 - y2]

-- | Calculates the coordinates that have distance 1 to the origin coordinate using the Chebyshev metric.
chebyshevNeighbours :: Coordinate -> Set Coordinate
chebyshevNeighbours (x, y) = Set.fromList [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]

-- | Calculates the coordinates that have distance 1 to the origin coordinate using the Euclidean metric.
euclideanNeighbours :: Coordinate -> Set Coordinate
euclideanNeighbours (x, y) = Set.fromList [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]

-- | Gets whether the given coordinate is within the bounds of the grid.
inBounds :: Grid a -> Coordinate -> Bool
inBounds grid (r, c) = r >= 0 && c >= 0 && r < length grid && c < length (grid !! r)

-- | Calculates the Manhattan distance between two coordinates.
manhattan :: Coordinate -> Coordinate -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Gets whether two coordinates are orthogonally placed.
--   If both coordinates are equal, they are considered orthogonal.
orthogonal :: Coordinate -> Coordinate -> Bool
orthogonal (ax, ay) (bx, by) = ax == bx || ay == by

-- | Renders the contents of the grid to stdout.
render :: (a -> Char) -> Grid a -> IO ()
render encoder = mapM_ (putStrLn . map encoder)
