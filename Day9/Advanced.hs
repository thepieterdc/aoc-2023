module Day9.Advanced where

import Day9.Common (nextNumber)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sum . map (nextNumber . reverse . map read . words) . lines
