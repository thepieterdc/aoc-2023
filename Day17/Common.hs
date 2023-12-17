module Day17.Common where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Utils.Grid (Coordinate, Direction (..), Grid, get, inBounds, move, rotateClockwise, rotateCounterClockwise)
import Utils.Heap (Heap, getMin, insert, removeMin, singleton)

data Tile = Tile {coordinate :: Coordinate, direction :: Direction, directionCount :: Int, loss :: Int} deriving (Eq, Show)

instance Ord Tile where
  compare :: Tile -> Tile -> Ordering
  compare t1 t2 = compare (loss t1) (loss t2)

hash :: Tile -> String
hash (Tile coord dir dc _) = show coord ++ show dir ++ show dc

nextTiles :: Int -> Int -> Grid Int -> Tile -> Int -> [Tile]
nextTiles minDC maxDC grid here loss = filter (\t -> inBounds grid (coordinate t) && ((direction t == dir && directionCount t <= maxDC) || (direction t /= dir && minDC <= dc))) [rotateClock, rotateCounterClock, continue]
  where
    coord = coordinate here
    dir = direction here
    dc = directionCount here

    clockCounterD = rotateCounterClockwise dir
    clockD = rotateClockwise dir
    rotateClock = Tile (move clockD coord) clockD 1 loss
    rotateCounterClock = Tile (move clockCounterD coord) clockCounterD 1 loss
    continue = Tile (move dir coord) dir (dc + 1) loss

parse :: String -> Grid Int
parse input = map (map (\c -> read [c])) $ lines input

solve :: Int -> Int -> Grid Int -> Int
solve minDC maxDC grid = walk minDC maxDC grid todo Map.empty finish
  where
    east = Tile (0, 1) East 1 0
    south = Tile (1, 0) South 1 0
    todo = insert east (singleton south)

    finish = (length grid - 1, length (head grid) - 1)

walk :: Int -> Int -> Grid Int -> Heap Tile -> Map String Int -> Coordinate -> Int
walk _ _ _ todo seen _ | isNothing (getMin todo) = error (show seen)
walk _ _ grid todo _ finish | coordinate (fromJust $ getMin todo) == finish = get grid finish + loss (fromJust $ getMin todo)
walk minDC maxDC grid todo seen finish = walk minDC maxDC grid next (Map.insert cacheKey (loss $ fromJust current) seen) finish
  where
    (current, todo') = removeMin todo
    current' = fromJust current
    newLoss = loss current' + get grid (coordinate current')
    cacheKey = hash current'
    inCache = Map.lookup cacheKey seen
    next = if isJust inCache && (fromJust inCache <= newLoss) then todo' else foldl (flip insert) todo' (nextTiles minDC maxDC grid (fromJust current) newLoss)
