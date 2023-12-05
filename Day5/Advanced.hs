module Day5.Advanced where

import           Day5.Common (Almanac (seeds), parse, solve)
import           Utils.IO    (loadInput)

actualSeeds :: Almanac -> [Int]
actualSeeds almanac = actualSeeds' (seeds almanac) where
    actualSeeds' :: [Int] -> [Int]
    actualSeeds' [] = []
    actualSeeds' list = resolved ++ actualSeeds' (drop 2 list) where
        startEnd = take 2 list
        resolved = [head startEnd .. (head startEnd + last startEnd - 1)]

main :: IO ()
main = loadInput >>= print . solve actualSeeds . parse
