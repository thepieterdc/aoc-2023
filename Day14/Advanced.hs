module Day14.Simple where

import Data.Map (Map)
import qualified Data.Map as Map (adjustWithKey, empty, insert, lookup)
import Data.Maybe (fromJust, isJust)
import Day14.Common (Grid, Rock (..), parse, score, tiltEast, tiltNorth, tiltSouth, tiltWest)
import Utils.IO (loadInput)

cycleGrid :: Grid -> Grid
cycleGrid grid = tiltEast $ tiltSouth $ tiltWest $ tiltNorth grid

run :: Int -> [Int] -> Map [Int] Int -> Grid -> Int
run 0 _ _ grid = score grid
run 1 _ _ grid = score $ cycleGrid grid
run left lastTenScores cache grid = if isJust cacheHit then run (left `mod` (fromJust cacheHit - (left - 1))) [] cache' cycled else run (left - 1) cacheKey cache' cycled
  where
    cycled = cycleGrid grid
    thisScore = score cycled
    cacheKey = thisScore : take 9 lastTenScores
    cacheHit = Map.lookup cacheKey cache
    cache' = Map.insert cacheKey (left - 1) cache

main :: IO ()
main = loadInput >>= print . run (1000000000 - 2) [] Map.empty . parse

--
