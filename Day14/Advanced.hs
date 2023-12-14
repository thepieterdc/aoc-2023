module Day14.Simple where

import Data.Map (Map)
import qualified Data.Map as Map (adjustWithKey, empty, insert, lookup)
import Data.Maybe (fromJust, isJust)
import Day14.Common (Grid, Rock (..), parse, score, tiltEast, tiltNorth, tiltSouth, tiltWest)
import Utils.IO (loadInput)

-- run :: Int -> [Int] -> Map String Int -> Grid -> Int
run 0 lastFourScores _ grid = lastFourScores
run times lastFourScores cache grid = if isJust fromCache then run (fromJust fromCache - times - 1) [] Map.empty cycledGrid else run (times - 1) (thisScore : take 3 lastFourScores) updatedCache cycledGrid
  where
    cycledGrid = tiltEast $ tiltSouth $ tiltWest $ tiltNorth grid
    thisScore = score cycledGrid
    fromCache = Map.lookup (show lastFourScores ++ show thisScore) cache
    updatedCache = Map.insert (show lastFourScores ++ show thisScore) times cache

main :: IO ()
main = loadInput >>= print . run 1000000 [] Map.empty . parse
