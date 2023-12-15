module Day15.Simple where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Day15.Common (hash)
import Utils.IO (loadInput)

main :: IO ()
main = loadInput >>= print . sum . map hash . splitOn "," . head . lines
