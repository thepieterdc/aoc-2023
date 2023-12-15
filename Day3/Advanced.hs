module Day3.Advanced where

import Data.Set (Set)
import qualified Data.Set as Set
import Day3.Common (Item (..), mergeWithCoordinates, parse)
import Utils.Grid (Coordinate, chebyshevNeighbours)
import Utils.IO (loadInput)

splitGears :: Set Coordinate -> [(Set Coordinate, Int)] -> [(Coordinate, Coordinate, Item)] -> ([Coordinate], [(Set Coordinate, Int)])
splitGears gears parts ((start, end, Gear) : rest) = splitGears (Set.insert start gears) parts rest
splitGears gears parts ((_, _, Symbol) : rest) = splitGears gears parts rest
splitGears gears parts (((row, startCol), (_, endCol), Part num) : rest) = splitGears gears (parts ++ [(Set.fromList [(row, c) | c <- [startCol .. endCol]], read num)]) rest
splitGears gears parts [] = (Set.elems gears, parts)

usedGears :: [Coordinate] -> [(Set Coordinate, Int)] -> [Int]
usedGears (gear : gears) parts = usedGear gear : usedGears gears parts
  where
    usedGear :: Coordinate -> Int
    usedGear g = if length nbParts == 2 then product nbParts else 0
      where
        nbs = chebyshevNeighbours g
        nbParts = map snd (filter (not . Set.disjoint nbs . fst) parts)
usedGears [] _ = []

main :: IO ()
main = loadInput >>= print . sum . uncurry usedGears . splitGears Set.empty [] . concat . mergeWithCoordinates 0 . parse
