module Day1.Simple where

import           Utils.IO (loadInput)
import Data.Char (isDigit, digitToInt)
import Utils.Lists (maybeHead, maybeLast)
import Data.Maybe (catMaybes)

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

firstLast :: [Int] -> Int
firstLast [] = 0
firstLast xs = (head xs * 10) + last xs

main :: IO ()
main = loadInput >>= print . sum . map (firstLast . findDigits) . lines
