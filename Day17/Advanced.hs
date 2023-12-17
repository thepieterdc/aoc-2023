module Day17.Simple where

import Day17.Common (parse, solve)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . solve 4 10 . parse
