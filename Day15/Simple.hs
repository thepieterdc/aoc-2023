module Day15.Simple where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Utils.IO (loadInput)

hash :: String -> Int
hash chars = foldl (\acc c -> ((acc + c) * 17) `mod` 256) 0 (map ord chars)

main :: IO ()
main = loadInput >>= print . sum . map hash . splitOn "," . head . lines
