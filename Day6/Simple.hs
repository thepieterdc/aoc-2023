module Day6.Simple where

import           Day6.Common (solve)
import           Utils.IO    (loadInput)

main :: IO ()
main = loadInput >>= print . solve id
