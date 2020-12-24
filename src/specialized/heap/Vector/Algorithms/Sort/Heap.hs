{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Vector.Algorithms.Sort.Heap
-- Copyright   : (c) 2008-2015,2020 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
--
-- This module implements operations for working with a quaternary
-- heap stored in an unboxed array. Most heapsorts are defined in
-- terms of a binary heap, in which each internal node has at most two
-- children. By contrast, a quaternary heap has internal nodes with up
-- to four children. This reduces the number of comparisons in a
-- heapsort slightly, and improves locality (again, slightly) by
-- flattening out the heap.

module Vector.Algorithms.Sort.Heap
  ( -- * Sorting
    sort
  , sortBounds
  -- * Selection
  , select
  , selectBounds
  -- * Partial sorts
  , partialSort
  , partialSortBounds
  -- * Heap operations
  , heapify
  , pop
  , popTo
  , sortHeap
  , heapInsert
  ) where

import Prelude hiding (read, length, compare)

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST (ST)

import Data.Bits

import MVector
import Elem

import qualified Vector.Algorithms.Sort.Optimal as O

type RW = PrimState IO

-- | Sorts an entire array.
sort :: PrimMonad m => MVector (PrimState m) -> m ()
sort v = sortBounds v 0 (length v)
{-# inline sort #-}

-- | Sorts a portion of an array [l,u)
sortBounds
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
sortBounds a l u
  | len < 2   = return ()
  | len == 2  = O.sort2ByOffset a l
  | len == 3  = O.sort3ByOffset a l
  | len == 4  = O.sort4ByOffset a l
  | otherwise = heapify a l u >> sortHeap a l (l+4) u >> O.sort4ByOffset a l
 where len = u - l
{-# inlinable sortBounds #-}
{-# specialize sortBounds :: MVector s -> Int -> Int -> ST s () #-}
{-# specialize
    sortBounds :: MVector RW -> Int -> Int -> IO () #-}

-- | Moves the lowest k elements to the front of the array.
-- The elements will be in no particular order.
select
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ number of elements to select, k
  -> m ()
select a k = selectBounds a k 0 (length a)
{-# inlinable select #-}
{-# specialize select :: MVector s -> Int -> ST s () #-}
{-# specialize select :: MVector RW -> Int -> IO () #-}

-- | Moves the 'lowest' k elements in the portion [l,u) of the
-- array into the positions [l,k+l). The elements will be in
-- no particular order.
selectBounds
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ number of elements to select, k
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
selectBounds a k l u
  | l + k <= u = heapify a l (l + k) >> go l (l + k) (u - 1)
  | otherwise  = return ()
 where
 go l m u
   | u < m      = return ()
   | otherwise  = do el <- read a l
                     eu <- read a u
                     case compare eu el of
                       LT -> popTo a l m u
                       _  -> return ()
                     go l m (u - 1)
{-# inlinable selectBounds #-}
{-# specialize
    selectBounds :: MVector s -> Int -> Int -> Int -> ST s ()
  #-}
{-# specialize
    selectBounds :: MVector RW -> Int -> Int -> Int -> IO ()
  #-}

-- | Moves the lowest k elements to the front of the array, sorted.
--
-- The remaining values of the array will be in no particular order.
partialSort
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ number of elements to sort, k
  -> m ()
partialSort a k = partialSortBounds a k 0 (length a)
{-# inlinable partialSort #-}
{-# specialize partialSort :: MVector s -> Int -> ST s () #-}
{-# specialize partialSort :: MVector RW -> Int -> IO () #-}

-- | Moves the lowest k elements in the portion [l,u) of the array
-- into positions [l,k+l), sorted.
--
-- The remaining values in [l,u) will be in no particular order.
-- Values outside the range [l,u) will be unaffected.
partialSortBounds
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ number of elements to sort, k
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
partialSortBounds a k l u
  -- this potentially does more work than absolutely required,
  -- but using a heap to find the least 2 of 4 elements
  -- seems unlikely to be better than just sorting all of them
  -- with an optimal sort, and the latter is obviously index
  -- correct.
  | len <  2   = return ()
  | len == 2   = O.sort2ByOffset a l
  | len == 3   = O.sort3ByOffset a l
  | len == 4   = O.sort4ByOffset a l
  | u <= l + k = sortBounds a l u
  | otherwise  = do selectBounds a k l u
                    sortHeap a l (l + 4) (l + k)
                    O.sort4ByOffset a l
 where
 len = u - l
{-# inlinable partialSortBounds #-}
{-# specialize
    partialSortBounds :: MVector s -> Int -> Int -> Int -> ST s () #-}
{-# specialize
    partialSortBounds
      :: MVector RW -> Int -> Int -> Int -> IO () #-}

-- | Constructs a heap in a portion of an array [l, u), using the
-- values therein.
--
-- Note: 'heapify' is more efficient than constructing a heap by
-- repeated insertion. Repeated insertion has complexity O(n*log n)
-- while 'heapify' is able to construct a heap in O(n), where n is the
-- number of elements in the heap.
heapify
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m ()
heapify a l u = heapify0 a l len ((len - 1) `shiftR` 2)
  where len = u - l
{-# inline heapify #-}

heapify0
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ offset
  -> Int -- ^ size
  -> Int -- ^ remaining
  -> m ()
heapify0 a l len k
  | k < 0 = pure ()
  | otherwise = do
    e <- read a (l+k)
    siftByOffset a e l k len
    heapify0 a l len (k-1)
{-# inlineable heapify0 #-}
{-# specialize heapify0 :: MVector s -> Int -> Int -> Int -> ST s () #-}
{-# specialize heapify0 :: MVector RW -> Int -> Int -> Int -> IO () #-}

-- | Given a heap stored in a portion of an array [l,u), swaps the
-- top of the heap with the element at u and rebuilds the heap.
pop
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> m ()
pop a l u = popTo a l u u
{-# inline pop #-}

-- | Given a heap stored in a portion of an array [l,u) swaps the top
-- of the heap with the element at position t, and rebuilds the heap.
popTo
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> Int -- ^ index to pop to, t
  -> m ()
popTo a l u t = do
  al <- read a l
  at <- read a t
  write a t al
  siftByOffset a at l 0 (u - l)
{-# inline popTo #-}

-- | Given a heap stored in a portion of an array [l,u), sorts the
-- highest values into [m,u). The elements in [l,m) are not in any
-- particular order.
sortHeap
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ lower heap index, l
  -> Int -- ^ lower bound of final sorted portion, m
  -> Int -- ^ upper heap index, u
  -> m ()
sortHeap a l m u
  | m < k = pop a l k >> sortHeap a l m k
  | otherwise = swap a l m
  where k = u-1
{-# inlinable sortHeap #-}
{-# specialize sortHeap :: MVector s -> Int -> Int -> Int -> ST s () #-}
{-# specialize
    sortHeap :: MVector RW -> Int -> Int -> Int -> IO () #-}

-- | Given a heap stored in a portion of an array [l,u) and an element e,
-- inserts the element into the heap, resulting in a heap in [l,u].
--
-- Note: it is best to only use this operation when incremental construction of
-- a heap is required. 'heapify' is capable of building a heap in O(n) time,
-- while repeated insertion takes O(n*log n) time.
heapInsert
  :: PrimMonad m
  => MVector (PrimState m)
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> Elem -- ^ element to be inserted, e
  -> m ()
heapInsert v l u e = sift (u - l)
 where
 sift k
   | k <= 0    = write v l e
   | otherwise = let pi = shiftR (k-1) 2
                  in read v (l + pi) >>= \p -> case compare p e of
                       LT -> write v (l + k) p >> sift pi
                       _  -> write v (l + k) e
{-# inlinable heapInsert #-}
{-# specialize
    heapInsert :: MVector s -> Int -> Int -> Elem -> ST s () #-}
{-# specialize
    heapInsert :: MVector RW -> Int -> Int -> Elem -> IO () #-}

-- Rebuilds a heap with a hole in it from start downwards. Afterward,
-- the heap property should apply for [start + off, len + off). val
-- is the new value to be put in the hole.
siftByOffset
  :: PrimMonad m
  => MVector (PrimState m) -> Elem -> Int -> Int -> Int -> m ()
siftByOffset !a !val !off root len
  | child < len = do
    (!child', ac) <- maximumChild a off child len
    case compare val ac of
      LT -> write a (root+off) ac >> siftByOffset a val off child' len
      _  -> write a (root+off) val
  | otherwise = write a (root+off) val
  where child = root `shiftL` 2 + 1
{-# inlinable siftByOffset #-}
{-# specialize
    siftByOffset :: MVector s -> Elem -> Int -> Int -> Int -> ST s () #-}
{-# specialize
    siftByOffset :: MVector RW -> Elem -> Int -> Int -> Int -> IO () #-}

-- Finds the maximum child of a heap node, given the indx of the first
-- child.
maximumChild
  :: PrimMonad m
  => MVector (PrimState m) -> Int -> Int -> Int -> m (Int,  Elem)
maximumChild a off child1 len
  | child4 < len = do ac1 <- read a (child1 + off)
                      ac2 <- read a (child2 + off)
                      ac3 <- read a (child3 + off)
                      ac4 <- read a (child4 + off)
                      return $ case compare ac1 ac2 of
                                 LT -> case compare ac2 ac3 of
                                         LT -> case compare ac3 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child3, ac3)
                                         _  -> case compare ac2 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child2, ac2)
                                 _  -> case compare ac1 ac3 of
                                         LT -> case compare ac3 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child3, ac3)
                                         _  -> case compare ac1 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child1, ac1)
  | child3 < len = do ac1 <- read a (child1 + off)
                      ac2 <- read a (child2 + off)
                      ac3 <- read a (child3 + off)
                      return $ case compare ac1 ac2 of
                                 LT -> case compare ac2 ac3 of
                                         LT -> (child3, ac3)
                                         _  -> (child2, ac2)
                                 _  -> case compare ac1 ac3 of
                                         LT -> (child3, ac3)
                                         _  -> (child1, ac1)
  | child2 < len = do ac1 <- read a (child1 + off)
                      ac2 <- read a (child2 + off)
                      return $ case compare ac1 ac2 of
                                 LT -> (child2, ac2)
                                 _  -> (child1, ac1)
  | otherwise    = do ac1 <- read a (child1 + off) ; return (child1, ac1)
 where
 !child2 = child1 + 1
 !child3 = child1 + 2
 !child4 = child1 + 3
{-# inline maximumChild #-}
