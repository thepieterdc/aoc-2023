module Day9.Simple where

import Day9.Common (nextNumber, parse)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sum . map nextNumber . parse
