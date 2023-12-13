module Day9.Simple where

import Day9.Common (nextNumber)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sum . map (nextNumber . map read . words) . lines
