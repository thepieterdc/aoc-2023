module Day7.Simple where

import Data.Char (isDigit)
import Data.List (sort)
import Day7.Common (parse, score)
import Utils.IO (loadInput)

letterToInt :: Char -> Int
letterToInt 'A' = 14
letterToInt 'K' = 13
letterToInt 'Q' = 12
letterToInt 'J' = 11
letterToInt 'T' = 10
letterToInt d = read [d] :: Int

main :: IO ()
main = loadInput >>= print . sum . zipWith (curry (\p -> fst p * score (snd p))) [1 ..] . sort . parse letterToInt
