-- |
-- Module      : Utils.Number
-- Description : Contains methods to handle working with numbers.
-- Copyright   : (c) Pieter De Clercq, 2023
-- License     : MIT
--
-- Contains methods to handle working with numbers.
module Utils.Number (module Utils.Number) where

import Data.Char (isDigit, ord, toUpper)

-- | Converts a hexadecimal string to a decimal integer.
hexToDec :: String -> Int
hexToDec = foldl (\acc x -> acc * 16 + hexDigitToInt x) 0 . map toUpper
  where
    hexDigitToInt x
      | isDigit x = ord x - ord '0'
      | x >= 'A' && x <= 'F' = ord x - ord 'A' + 10
