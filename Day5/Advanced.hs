module Day5.Advanced where

import           Day5.Common (Almanac (seeds), SeedRange, parse, solve)
import           Utils.IO    (loadInput)

resolveSeeds :: Almanac -> [SeedRange]
resolveSeeds almanac = resolve' (seeds almanac) where
    resolve' :: [Int] -> [SeedRange]
    resolve' [] = []
    resolve' list = resolved : resolve' (drop 2 list) where
        startEnd = take 2 list
        resolved = (head startEnd, head startEnd + last startEnd - 1)

main :: IO ()
main = loadInput >>= print . solve resolveSeeds . parse
