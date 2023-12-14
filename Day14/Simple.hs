module Day14.Simple where

import Day14.Common (parse, score, tiltNorth)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . score . tiltNorth . parse
