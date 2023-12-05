module Day5.Simple where

import           Day5.Common (Almanac (seeds), parse, solve)
import           Utils.IO    (loadInput)

main :: IO ()
main = loadInput >>= print . solve seeds . parse
