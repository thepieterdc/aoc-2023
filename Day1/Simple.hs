module Day1.Simple where

import           Data.Char   (digitToInt, isDigit)
import           Day1.Common (process)
import           Utils.IO    (loadInput)

main :: IO ()
main = loadInput >>= print . process (map digitToInt . filter isDigit)
