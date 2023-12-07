module Day5.Simple where

import Day5.Common (Almanac (seeds), SeedRange, parse, solve)
import Utils.IO (loadInput)

resolveSeeds :: Almanac -> [SeedRange]
resolveSeeds almanac = map (\s -> (s, s)) (seeds almanac)

main :: IO ()
main = loadInput >>= print . solve resolveSeeds . parse
