module Day1.Simple where

import           Utils.IO (loadInput)
import Data.Char (isDigit)
import Utils.Lists (maybeHead, maybeLast)
import Data.Maybe (catMaybes)

processLine :: String -> Integer
processLine line = read (catMaybes [maybeHead numbers, maybeLast numbers]) :: Integer
    where numbers = filter isDigit line

main :: IO ()
main = loadInput >>= print . sum . map processLine . lines
