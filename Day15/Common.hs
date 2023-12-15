module Day15.Common where

import Data.Char (ord)

hash :: String -> Int
hash chars = foldl (\acc c -> ((acc + c) * 17) `mod` 256) 0 (map ord chars)
