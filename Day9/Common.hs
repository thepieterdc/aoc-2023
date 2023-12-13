module Day9.Common where

nextNumber :: [Int] -> Int
nextNumber nums | all (== 0) nums = 0
nextNumber nums = last nums + nextNumber (zipWith (-) (drop 1 nums) nums)
