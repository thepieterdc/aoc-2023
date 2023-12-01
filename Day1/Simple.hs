module Day1.Simple where

import           Data.Char   (digitToInt, isDigit)
import           Data.Maybe  (catMaybes)
import           Day1.Common (process)
import           Utils.IO    (loadInput)
import           Utils.Lists (maybeHead, maybeLast)

main :: IO ()
main = loadInput >>= print . process (map digitToInt . filter isDigit)
