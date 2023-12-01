module Day1.Simple where

import           Data.Char   (isDigit)
import           Data.Maybe  (catMaybes)
import           Utils.IO    (loadInput)
import           Utils.Lists (maybeHead, maybeLast)

processLine :: String -> Integer
processLine line = read (catMaybes [maybeHead numbers, maybeLast numbers]) :: Integer
    where numbers = filter isDigit line

main :: IO ()
main = loadInput >>= print . sum . map processLine . lines
