module Day7.Simple where

import           Utils.IO (loadInput)
import Day7.Common (parse, Hand (score), findHandType)
import Data.List (sort)

main :: IO ()
-- main = loadInput >>= print . sum . zipWith (curry (\ p -> fst p * snd (snd p))) [1 .. ] . sort . map (\h -> (findHandType h, score h)) . parse
main = loadInput >>= print . sort . map (\h -> (findHandType h, score h)) . parse
