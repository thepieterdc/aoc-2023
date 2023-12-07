module Day6.Simple where

import Data.List (intercalate)
import Day6.Common (Race (Race, distance, time), solve)
import Utils.IO (loadInput)

merge :: [Race] -> Race
merge races = Race times distances
  where
    times = read (intercalate "" $ map (show . time) races) :: Int
    distances = read (intercalate "" $ map (show . distance) races) :: Int

main :: IO ()
main = loadInput >>= print . solve (\rs -> [merge rs])
