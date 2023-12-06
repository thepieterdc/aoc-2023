module Day6.Simple where

import           Day6.Common (parse)
import           Utils.IO    (loadInput)

main :: IO ()
main = loadInput >>= print . parse
