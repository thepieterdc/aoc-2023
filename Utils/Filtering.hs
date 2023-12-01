{-|
Module      : Utils.Filtering
Description : Contains reusable filtering predicates and methods.
Copyright   : (c) Pieter De Clercq, 2022
License     : MIT

Contains reusable filtering predicates and methods.
-}
module Utils.Filtering (module Utils.Filtering) where
import           Data.Char (isDigit)

-- |Counts the amount of items in the list that match the given predicate.
countWhere :: (a -> Bool) -> [a] -> Int
countWhere fn items = length $ filter fn items

-- |Validates whether the given String is a number.
isNumber :: String -> Bool
isNumber "" = False
isNumber a  = all isDigit a
