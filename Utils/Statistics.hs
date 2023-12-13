-- |
-- Module      : Utils.Parser
-- Description : Contains methods to handle statistical operations.
-- Copyright   : (c) Pieter De Clercq, 2023
-- License     : MIT
--
-- Contains methods to parse strings into usable structures.
module Utils.Statistics (combinations) where

import Data.List (subsequences)

-- | Generates the combinations of k elements from a set of n elements.
combinations :: Int -> [a] -> [[a]]
combinations k ns = filter (\x -> length x == k) $ subsequences ns
