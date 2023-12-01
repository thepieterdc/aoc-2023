module Day1.Simple where

import           Data.Char   (digitToInt, isDigit)
import           Data.Maybe  (catMaybes)
import           Day1.Common (process)
import           Utils.IO    (loadInput)
import           Utils.Lists (maybeHead, maybeLast)

findDigits :: String -> [Int]
findDigits ('o' : 'n' : 'e' : rest) = 1 : findDigits ("ne" ++ rest)
findDigits ('t' : 'w' : 'o' : rest) = 2 : findDigits ("wo" ++ rest)
findDigits ('t' : 'h' : 'r' : 'e' : 'e' : rest) = 3 : findDigits ("hree" ++ rest)
findDigits ('f' : 'o' : 'u' : 'r' : rest) = 4 : findDigits ("our" ++ rest)
findDigits ('f' : 'i' : 'v' : 'e' : rest) = 5 : findDigits ("ive" ++ rest)
findDigits ('s' : 'i' : 'x' : rest) = 6 : findDigits ("ix" ++ rest)
findDigits ('s' : 'e' : 'v' : 'e' : 'n' : rest) = 7 : findDigits ("even" ++ rest)
findDigits ('e' : 'i' : 'g' : 'h' : 't' : rest) = 8 : findDigits ("ight" ++ rest)
findDigits ('n' : 'i' : 'n' : 'e' : rest) = 9 : findDigits ("ine" ++ rest)
findDigits (c:rest) = [digitToInt c | isDigit c] ++ findDigits rest
findDigits _ = []

main :: IO ()
main = loadInput >>= print . process findDigits
