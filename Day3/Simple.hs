module Day3.Simple where

import Data.Set (Set)
import qualified Data.Set as Set
import Day3.Common (Item (..), mergeWithCoordinates, parse)
import Utils.Grid (Coordinate, chebyshevNeighbours)
import Utils.IO (loadInput)

splitSymbols :: Set Coordinate -> [([Coordinate], Int)] -> [(Coordinate, Coordinate, Item)] -> (Set Coordinate, [([Coordinate], Int)])
splitSymbols symbols parts ((start, end, Gear) : rest) = splitSymbols (Set.insert start symbols) parts rest
splitSymbols symbols parts ((start, end, Symbol) : rest) = splitSymbols (Set.insert start symbols) parts rest
splitSymbols symbols parts (((row, startCol), (_, endCol), Part num) : rest) = splitSymbols symbols (parts ++ [([(row, c) | c <- [startCol .. endCol]], read num)]) rest
splitSymbols symbols parts [] = (symbols, parts)

sumParts :: Set Coordinate -> [([Coordinate], Int)] -> [Int]
sumParts symbols (part : parts) = sumPart part ++ sumParts symbols parts
  where
    sumPart :: ([Coordinate], Int) -> [Int]
    sumPart (coordinates, value) = [value | not (all (Set.disjoint symbols . chebyshevNeighbours) coordinates)]
sumParts symbols [] = []

main :: IO ()
main = loadInput >>= print . sum . uncurry sumParts . splitSymbols Set.empty [] . concat . mergeWithCoordinates 0 . parse
