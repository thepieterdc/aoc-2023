module Day17.Simple where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Utils.Grid (Coordinate, Direction (East, South), Grid, get, inBounds, move, rotateClockwise, rotateCounterClockwise)
import Utils.Heap (Heap, empty, fromList, getMin, insert, removeMin, singleton)
import Utils.IO (loadInput)

data Tile = Tile {coordinate :: Coordinate, direction :: Direction, directionCount :: Int, loss :: Int} deriving (Eq, Show)

instance Ord Tile where
  compare :: Tile -> Tile -> Ordering
  compare t1 t2 = compare (loss t1) (loss t2)

hash :: Tile -> String
hash (Tile coord dir dc _) = show coord ++ show dir ++ show dc

nextTiles :: Grid Int -> Tile -> Int -> [Tile]
nextTiles grid here loss = filter (\t -> inBounds grid (coordinate t) && directionCount t <= 3) [rotateClock, rotateCounterClock, continue]
  where
    coord = coordinate here

    clockCounterD = rotateCounterClockwise (direction here)
    clockD = rotateClockwise (direction here)
    rotateClock = Tile (move clockD coord) clockD 1 loss
    rotateCounterClock = Tile (move clockCounterD coord) clockCounterD 1 loss
    continue = Tile (move (direction here) coord) (direction here) (directionCount here + 1) loss

parse :: String -> Grid Int
parse input = map (map (\c -> read [c])) $ lines input

solve :: Grid Int -> Int
solve grid = walk grid todo Map.empty finish
  where
    east = Tile (0, 1) East 1 (get grid (0, 1))
    south = Tile (1, 0) South 1 (get grid (0, 1))
    todo = insert east (singleton south)

    finish = (length grid - 1, length (head grid) - 1)

walk :: Grid Int -> Heap Tile -> Map String Int -> Coordinate -> Int
walk grid todo seen finish | isNothing (getMin todo) = error (show seen)
walk grid todo seen finish | coordinate (fromJust $ getMin todo) == finish = loss $ fromJust $ getMin todo
walk grid todo seen finish = walk grid next (Map.insert cacheKey (loss $ fromJust current) seen) finish
  where
    (current, todo') = removeMin todo
    current' = fromJust current
    newLoss = loss current' + get grid (coordinate current')
    cacheKey = maybe "" hash current
    inCache = Map.lookup cacheKey seen
    next = if isJust inCache && (fromJust inCache <= newLoss) then todo' else foldl (flip insert) todo' (nextTiles grid (fromJust current) newLoss)

main :: IO ()
main = loadInput >>= print . solve . parse
