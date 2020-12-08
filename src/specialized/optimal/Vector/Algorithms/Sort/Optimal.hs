-- |
-- Module      : Vector.Algorithms.Sort.Optimal
-- Copyright   : (c) 2008-2020 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
--
-- Optimal sorts for very small array sizes, or for small numbers of
-- particular indices in a larger array (to be used, for instance, for
-- sorting a median of 3 values into the lowest position in an array
-- for a median-of-3 quicksort).

-- The code herein was adapted from a C algorithm for optimal sorts
-- of small arrays. The original code was produced for the article
-- /Sorting Revisited/ by Paul Hsieh, available here:
--
--   http://www.azillionmonkeys.com/qed/sort.html
--
-- The LICENSE file contains the relevant copyright information for
-- the reference C code.

module Vector.Algorithms.Sort.Optimal
  ( sort2ByIndex
  , sort2ByOffset
  , sort3ByIndex
  , sort3ByOffset
  , sort4ByIndex
  , sort4ByOffset
  ) where

import Prelude hiding (read, Ord(..))

import Control.Monad.Primitive (PrimMonad(PrimState))

import Elem
import MVector

-- | Sorts the elements at the positions 'off' and 'off + 1' in the
-- given array using the comparison.
sort2ByOffset :: PrimMonad m => MVector (PrimState m) -> Int -> m ()
sort2ByOffset a off = sort2ByIndex a off (off + 1)

-- | Sorts the elements at the two given indices using the comparison.
-- This is essentially a compare-and-swap, although the first index is
-- assumed to be the 'lower' of the two.
sort2ByIndex :: PrimMonad m => MVector (PrimState m) -> Int -> Int -> m ()
sort2ByIndex a i j = do
  a0 <- read a i
  a1 <- read a j
  case compare a0 a1 of
    GT -> write a i a1 >> write a j a0
    _  -> return ()

-- | Sorts the three elements starting at the given offset in the array.
sort3ByOffset :: PrimMonad m => MVector (PrimState m) -> Int -> m ()
sort3ByOffset a off = sort3ByIndex a off (off + 1) (off + 2)

-- | Sorts the elements at the three given indices. The indices are
-- assumed to be given from lowest to highest, so if 'l < m < u' then
-- 'sort3ByIndex a m l u' essentially sorts the median of three into
-- the lowest position in the array.
sort3ByIndex
  :: PrimMonad m =>  MVector (PrimState m) -> Int -> Int -> Int -> m ()
sort3ByIndex a i j k = do
  a0 <- read a i
  a1 <- read a j
  a2 <- read a k
  case compare a0 a1 of
    GT -> case compare a0 a2 of
      GT -> case compare a2 a1 of
        LT -> do write a i a2
                 write a k a0
        _  -> do write a i a1
                 write a j a2
                 write a k a0
      _  -> do write a i a1
               write a j a0
    _  -> case compare a1 a2 of
      GT -> case compare a0 a2 of
        GT -> do write a i a2
                 write a j a0
                 write a k a1
        _  -> do write a j a2
                 write a k a1
      _  -> return ()

-- | Sorts the four elements beginning at the offset.
sort4ByOffset :: PrimMonad m => MVector (PrimState m) -> Int -> m ()
sort4ByOffset a off
  = sort4ByIndex a off (off + 1) (off + 2) (off + 3)

-- The horror...

-- | Sorts the elements at the four given indices. Like the 2 and 3
-- element versions, this assumes that the indices are given in
-- increasing order, so it can be used to sort medians into particular
-- positions and so on.
sort4ByIndex
  :: PrimMonad m
  => MVector (PrimState m) -> Int -> Int -> Int -> Int -> m ()
sort4ByIndex a i j k l = do
  a0 <- read a i
  a1 <- read a j
  a2 <- read a k
  a3 <- read a l
  case compare a0 a1 of
    GT -> case compare a0 a2 of
      GT -> case compare a1 a2 of
        GT -> case compare a1 a3 of
          GT -> case compare a2 a3 of
            GT -> do write a i a3
                     write a j a2
                     write a k a1
                     write a l a0
            _  -> do write a i a2
                     write a j a3
                     write a k a1
                     write a l a0
          _  -> case compare a0 a3 of
            GT -> do write a i a2
                     write a j a1
                     write a k a3
                     write a l a0
            _  -> do write a i a2
                     write a j a1
                     write a k a0
                     write a l a3
        _ -> case compare a2 a3 of
          GT -> case compare a1 a3 of
            GT -> do write a i a3
                     write a j a1
                     write a k a2
                     write a l a0
            _  -> do write a i a1
                     write a j a3
                     write a k a2
                     write a l a0
          _  -> case compare a0 a3 of
            GT -> do write a i a1
                     write a j a2
                     write a k a3
                     write a l a0
            _  -> do write a i a1
                     write a j a2
                     write a k a0
                     -- write a l a3
      _  -> case compare a0 a3 of
        GT -> case compare a1 a3 of
          GT -> do write a i a3
                   -- write a j a1
                   write a k a0
                   write a l a2
          _  -> do write a i a1
                   write a j a3
                   write a k a0
                   write a l a2
        _  -> case compare a2 a3 of
          GT -> do write a i a1
                   write a j a0
                   write a k a3
                   write a l a2
          _  -> do write a i a1
                   write a j a0
                   -- write a k a2
                   -- write a l a3
    _  -> case compare a1 a2 of
      GT -> case compare a0 a2 of
        GT -> case compare a0 a3 of
          GT -> case compare a2 a3 of
            GT -> do write a i a3
                     write a j a2
                     write a k a0
                     write a l a1
            _  -> do write a i a2
                     write a j a3
                     write a k a0
                     write a l a1
          _  -> case compare a1 a3 of
            GT -> do write a i a2
                     write a j a0
                     write a k a3
                     write a l a1
            _  -> do write a i a2
                     write a j a0
                     write a k a1
                     -- write a l a3
        _  -> case compare a2 a3 of
          GT -> case compare a0 a3 of
            GT -> do write a i a3
                     write a j a0
                     -- write a k a2
                     write a l a1
            _  -> do -- write a i a0
                     write a j a3
                     -- write a k a2
                     write a l a1
          _  -> case compare a1 a3 of
            GT -> do -- write a i a0
                     write a j a2
                     write a k a3
                     write a l a1
            _  -> do -- write a i a0
                     write a j a2
                     write a k a1
                     -- write a l a3
      _  -> case compare a1 a3 of
        GT -> case compare a0 a3 of
          GT -> do write a i a3
                   write a j a0
                   write a k a1
                   write a l a2
          _  -> do -- write a i a0
                   write a j a3
                   write a k a1
                   write a l a2
        _  -> case compare a2 a3 of
          GT -> do -- write a i a0
                   -- write a j a1
                   write a k a3
                   write a l a2
          _  -> do -- write a i a0
                   -- write a j a1
                   -- write a k a2
                   -- write a l a3
                   return ()
