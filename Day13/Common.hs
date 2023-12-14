module Day13.Common where

import Data.List (transpose)
import Data.Maybe (fromJust, fromMaybe)
import Utils.Lists (maybeHead)
import Utils.Maybe (mapMaybe)

type Grid = [String]

parse :: String -> [Grid]
parse input = parse' [] (lines input)
  where
    parse' :: Grid -> [String] -> [Grid]
    parse' grid [] = [grid]
    parse' grid (l : ls)
      | l == "" = grid : parse' [] ls
      | otherwise = parse' (grid ++ [l]) ls

diff :: String -> String -> Int
diff a b = length [x | (x, y) <- zip a b, x /= y]

reflect :: Int -> Grid -> Maybe Int
reflect threshold grid = maybeHead [cursor | cursor <- [1 .. end - 1], reflect' cursor]
  where
    end = length $ head grid

    reflect' :: Int -> Bool
    reflect' cursor = sum [diff (take width (drop leftStart row)) (reverse (take width $ drop (leftStart + width) row)) | row <- grid] == threshold
      where
        width = min cursor (end - cursor)
        leftStart = max 0 cursor - width
        rightEnd = min end cursor + width

solve :: Int -> Grid -> Int
solve threshold grid = fromMaybe (fromJust horizontal) vertical
  where
    vertical = reflect threshold grid
    horizontal = mapMaybe (100 *) $ reflect threshold $ transpose grid
