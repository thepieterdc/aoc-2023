module Day13.Simple where

import Day13.Common (parse, solve)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sum . map (solve 0) . parse
