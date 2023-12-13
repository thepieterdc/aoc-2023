module Day10.Advanced where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Ord (compare)
import qualified Data.Set as Set
import Day10.Common (Grid, Tile (..), findStart, parse, replacedGrids, startPrevious, walk)
import Utils.IO (loadInput)

countInside :: Grid -> Int
countInside grid = sum $ map (fst . (\r -> foldl (detectInside r) (0, False) (zip [0 ..] r)) . filter (/= Horizontal)) grid
  where
    detectInside :: [Tile] -> (Int, Bool) -> (Int, Tile) -> (Int, Bool)
    detectInside row (cnt, inside) (i, BendNW) | i > 0 && row !! (i - 1) == BendSE = (cnt, not inside)
    detectInside row (cnt, inside) (i, BendSW) | i > 0 && row !! (i - 1) == BendNE = (cnt, not inside)
    detectInside _ (cnt, inside) (_, Ground) = (cnt + (if inside then 1 else 0), inside)
    detectInside _ (cnt, inside) (_, Vertical) = (cnt, not inside)
    detectInside _ (cnt, inside) _ = (cnt, inside)

solve :: Grid -> Int
solve grid = countInside cleansedGrid
  where
    start@(sr, sc) = findStart 0 grid

    (startTile, replacedGrid) = head $ replacedGrids grid
    path = Set.fromList $ walk replacedGrid start (startPrevious start startTile) start

    cleansedGrid = [[if Set.member (ri, ci) path then c else Ground | (ci, c) <- zip [0 ..] r] | (ri, r) <- zip [0 ..] replacedGrid]

main :: IO ()
main = loadInput >>= print . solve . parse
