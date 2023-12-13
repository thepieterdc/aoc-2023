-- |
-- Module      : Utils.Parser
-- Description : Contains methods to handle statistical operations.
-- Copyright   : (c) Pieter De Clercq, 2023
-- License     : MIT
--
-- Contains methods to handle statistical operations.
module Utils.Statistics (choose) where

import Data.List (subsequences)

-- | Generates the combinations of k elements from a set of n elements.
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose k (x : xs) = map (x :) (choose (k - 1) xs) ++ choose k xs
