module Day11.Advanced where

import Day11.Common (expandCols, expandRows, parse, sums)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sums . expandCols 999999 . expandRows 999999 . parse
