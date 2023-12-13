module Day11.Common where

import Data.List (subsequences)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Grid (Coordinate, manhattan)
import Utils.Statistics (choose)

expand :: Int -> ((Int, Int) -> Int) -> (Int -> (Int, Int) -> (Int, Int)) -> [Coordinate] -> [Coordinate]
expand times find tf glxs = map (\p -> tf (times * emptyBefore (find p)) p) glxs
  where
    galaxyPts = Set.fromList $ map find glxs
    maxGalaxyPt = Set.findMax galaxyPts
    empties = Set.fromList [p | p <- [0 .. maxGalaxyPt], not (Set.member p galaxyPts)]

    emptyBefore :: Int -> Int
    emptyBefore p = Set.size $ Set.filter (< p) empties

expandCols :: Int -> [Coordinate] -> [Coordinate]
expandCols amt = expand amt snd (\delta (r, c) -> (r, c + delta))

expandRows :: Int -> [Coordinate] -> [Coordinate]
expandRows amt = expand amt fst (\delta (r, c) -> (r + delta, c))

parse :: String -> [Coordinate]
parse file = [(r, c) | (r, row) <- zip [0 ..] $ lines file, (c, v) <- zip [0 ..] row, v == '#']

sums :: [Coordinate] -> Int
sums coordinates = sum $ map (\c -> manhattan (head c) (last c)) $ choose 2 coordinates
