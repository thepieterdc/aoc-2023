module Day1.Common where

firstLast :: [Int] -> [Int]
firstLast [] = [0]
firstLast xs = [head xs * 10, last xs]

process :: (String -> [Int]) -> String -> Int
process digits ls = sum $ map (sum . firstLast . digits) (lines ls)
