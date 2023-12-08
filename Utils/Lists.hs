-- |
-- Module      : Utils.Lists
-- Description : Contains methods to operate on lists.
-- Copyright   : (c) Pieter De Clercq, 2022
-- License     : MIT
--
-- Contains methods to operate on lists.
module Utils.Lists (module Utils.Lists) where

import Control.Arrow ((&&&))
import Control.Monad (ap)
import Data.List (group, sort, unfoldr)
import Data.Maybe (listToMaybe)

endsWith :: Eq a => a -> [a] -> Bool
endsWith x = (== x) . last

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = map (head &&& length) . group . sort

-- | Splits the list in groups of the given size.
groupBySize :: Int -> [a] -> [[a]]
groupBySize size = unfoldr $ listToMaybe . ap (>>) (return . splitAt size)

-- | Applies the given function to the element at the given position in the list.
mapIdx ::
  -- | The position in the list to map
  Int ->
  -- | The mapping function to execute
  (a -> a) ->
  -- | The input list
  [a] ->
  -- | The resulting list
  [a]
mapIdx pos f l = take pos l ++ f (l !! pos) : drop (pos + 1) l

-- | Gets the head of the list if the list is not empty.
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (a : as) = Just a

-- | Gets the last item of the list if the list is not empty.
maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast as = Just $ last as

-- | Counts the amount of Nothing elements in the given list of Maybes.
nothings :: [Maybe a] -> Int
nothings [] = 0
nothings (Nothing : rest) = 1 + nothings rest
nothings (_ : rest) = nothings rest
