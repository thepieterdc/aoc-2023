-- |
-- Module      : Utils.Interval
-- Description : Contains methods to handle working with intervals or ranges.
-- Copyright   : (c) Pieter De Clercq, 2022
-- License     : MIT
--
-- Contains methods to handle working with intervals or ranges.
module Utils.Interval (module Utils.Interval) where

-- | Definition of an interval. The Type is assumed to implement Ord.
type Interval a = (a, a)

-- | Checks whether the first interval fully contains the second one.
contains :: Ord a => Interval a -> Interval a -> Bool
contains (a, b) (c, d) = a <= c && d <= b

-- | Checks whether the interval contains the given value.
containsValue :: Ord a => Interval a -> a -> Bool
containsValue (start, end) val = start <= val && val <= end

-- | Checks whether the intervals intersect in any way.
overlaps :: Ord a => Interval a -> Interval a -> Bool
overlaps (a, b) (c, d) = (a >= c && a <= d) || (b >= c && b <= d) || (c >= a && c <= b) || (d <= a && d >= b)
