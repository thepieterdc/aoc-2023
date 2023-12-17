module Day16.Advanced where

import qualified Data.Set as Set
import Day16.Common (Mirror, count, parse, walk)
import Utils.Grid (Coordinate, Direction (..), Grid)
import Utils.IO (loadInput)

run :: Grid ([Direction], Maybe Mirror) -> Int
run grid = foldr (\s m -> max m $ count $ walk (Set.singleton s) grid) 0 starts
  where
    cols = length $ head grid
    rows = length grid

    fromBottom = [((rows - 1, c), North) | c <- [0 .. cols - 1]]
    fromLeft = [((r, 0), East) | r <- [0 .. rows - 1]]
    fromRight = [((r, cols - 1), West) | r <- [0 .. rows - 1]]
    fromTop = [((0, c), South) | c <- [0 .. cols - 1]]
    starts = fromBottom ++ fromLeft ++ fromRight ++ fromTop

main :: IO ()
main = loadInput >>= print . run . parse
