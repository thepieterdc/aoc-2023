-- |
-- Module      : Utils.Grid
-- Description : Contains methods to handle working with grids.
-- Copyright   : (c) Pieter De Clercq, 2022
-- License     : MIT
--
-- Contains methods to handle working with grids.
module Utils.Grid (module Utils.Grid) where

import Data.Bits (Bits (rotate))
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

-- | Gets the value at the given coordinate.
get :: Grid a -> Coordinate -> a
get grid (r, c) = grid !! r !! c

-- | Gets whether the given coordinate is within the bounds of the grid.
inBounds :: Grid a -> Coordinate -> Bool
inBounds grid (r, c) = r >= 0 && c >= 0 && r < length grid && c < length (grid !! r)

-- | Calculates the Manhattan distance between two coordinates.
manhattan :: Coordinate -> Coordinate -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Modifies the element in the given position on the grid.
mapCoordinate :: (a -> a) -> Grid a -> Coordinate -> Grid a
mapCoordinate modif grid (r, c) = take r grid ++ [take c (grid !! r) ++ [modif $ get grid (r, c)] ++ drop (c + 1) (grid !! r)] ++ drop (r + 1) grid

-- | Moves a coordinate in the given direction.
move :: Direction -> Coordinate -> Coordinate
move East (r, c) = (r, c + 1)
move North (r, c) = (r - 1, c)
move South (r, c) = (r + 1, c)
move West (r, c) = (r, c - 1)

-- | Gets whether two coordinates are orthogonally placed.
--   If both coordinates are equal, they are considered orthogonal.
orthogonal :: Coordinate -> Coordinate -> Bool
orthogonal (ax, ay) (bx, by) = ax == bx || ay == by

-- | Renders the contents of the grid to stdout.
render :: (a -> Char) -> Grid a -> IO ()
render encoder = mapM_ (putStrLn . map encoder)

-- | Rotates a direction clockwise.
rotateClockwise :: Direction -> Direction
rotateClockwise East = South
rotateClockwise North = East
rotateClockwise South = West
rotateClockwise West = North

-- | Rotates a direction counter-clockwise.
rotateCounterClockwise :: Direction -> Direction
rotateCounterClockwise East = North
rotateCounterClockwise North = West
rotateCounterClockwise South = East
rotateCounterClockwise West = South
