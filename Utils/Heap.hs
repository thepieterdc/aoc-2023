-- |
-- Module      : Utils.Heap
-- Description : Contains methods to operate on a Heap.
-- Copyright   : (c) Pieter De Clercq, 2023
-- License     : MIT
--
-- Contains methods to operate on a heap.
module Utils.Heap (empty) where

import Control.Arrow (ArrowChoice (left))

type Rank = Int

data Heap a = Root | Node Rank a (Heap a) (Heap a) deriving (Eq, Show)

-- | Creates a new empty heap.
empty :: Heap a
empty = Root

-- | Restores the heap properties when appending or removing an element.
fixHeap :: a -> Heap a -> Heap a -> Heap a
fixHeap x left right = if rank left >= rank right then Node (rank right + 1) x left right else Node (rank left + 1) x right left

-- | Adds a new element into the heap.
insert :: Ord a => a -> Heap a -> Heap a
insert x = merge $ singleton x

-- | Merges two heaps together.
merge :: Ord a => Heap a -> Heap a -> Heap a
merge left Root = left
merge Root right = right
merge left@(Node _ x leftLeft leftRight) right@(Node _ y rightLeft rightRight) = if x <= y then fixHeap x left (merge leftRight right) else fixHeap y right (merge left rightRight)

-- | Returns the rank of the given node.
rank :: Heap a -> Rank
rank Root = 0
rank (Node r _ _ _) = r

-- | Removes the smallest element from the heap.
removeMin :: Ord a => Heap a -> Maybe (a, Heap a)
removeMin Root = Nothing
removeMin (Node _ x left right) = Just (x, merge left right)

-- | Creates a new heap with 1 element.
singleton :: a -> Heap a
singleton a = Node 1 a Root Root
