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

-- | Calculates the Chebyshev distance between two coordinates.
chebyshev :: Coordinate -> Coordinate -> Int
chebyshev (x1, y1) (x2, y2) = maximum $ map abs [x1 - x2, y1 - y2]

-- | Calculates the coordinates that have distance 1 to the origin coordinate using the Chebyshev metric.
chebyshevNeighbours :: Coordinate -> Set Coordinate
chebyshevNeighbours (x, y) = Set.fromList [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]

-- | Calculates the coordinates that have distance 1 to the origin coordinate using the Euclidean metric.
euclideanNeighbours :: Coordinate -> Set Coordinate
euclideanNeighbours (x, y) = Set.fromList [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]

-- | Gets whether two coordinates are orthogonally placed.
--   If both coordinates are equal, they are considered orthogonal.
orthogonal :: Coordinate -> Coordinate -> Bool
orthogonal (ax, ay) (bx, by) = ax == bx || ay == by
