module Day12.Simple where

import Day12.Common (parse, solutions)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sum . map solutions . parse
