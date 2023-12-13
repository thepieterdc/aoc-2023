module Day11.Simple where

import Day11.Common (expandCols, expandRows, parse, sums)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sums . expandCols 1 . expandRows 1 . parse
