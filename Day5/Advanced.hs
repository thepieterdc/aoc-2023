module Day5.Advanced where

import           Day5.Common (Almanac (seeds), SeedRange, parse, solve)
import           Utils.IO    (loadInput)

resolveSeeds :: Almanac -> [SeedRange]
resolveSeeds almanac = resolve' (seeds almanac) where
    resolve' :: [Int] -> [SeedRange]
    resolve' [] = []
    resolve' list = resolved : resolve' rest where
        (pair, rest) = splitAt 2 list
        resolved = (head pair, head pair + last pair - 1)

main :: IO ()
main = loadInput >>= print . solve resolveSeeds . parse
