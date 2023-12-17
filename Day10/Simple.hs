module Day10.Simple where

import Day10.Common (Tile, findStart, parse, replacedGrids, startPrevious, walk)
import Utils.Grid (Grid)
import Utils.IO (loadInput)

solve :: Grid Tile -> Int
solve grid = if odd solution then (solution + 1) `div` 2 else solution `div` 2
  where
    start@(sr, sc) = findStart 0 grid
    (startTile, replacedGrid) = head $ replacedGrids grid
    solution = length $ walk replacedGrid start (startPrevious start startTile) start

main :: IO ()
main = loadInput >>= print . solve . parse
