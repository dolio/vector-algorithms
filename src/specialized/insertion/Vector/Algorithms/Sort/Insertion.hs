{-# language BangPatterns #-}

-- |
-- Module      : Vector.Algorithms.Sort.Insertion
-- Copyright   : (c) 2008-2020 Dan Doel
-- Maintainer  : Dan Doel
-- Stability   : Experimental
--
-- A simple insertion sort. Though it's O(n^2), its iterative nature
-- can be beneficial for small arrays. It is used to sort small
-- segments of an array by some of the more heavy-duty, recursive
-- algorithms.

module Vector.Algorithms.Sort.Insertion
  ( sort
  , sortBounds
  , sortBounds'
  ) where

import Prelude hiding (read, length, compare)

import Elem
import MVector

import Control.Monad.ST (ST)
import Control.Monad.Primitive (PrimMonad(PrimState))

import qualified Vector.Algorithms.Sort.Optimal as O

-- | Sorts an entire array
sort :: PrimMonad m => MVector (PrimState m) -> m ()
sort a = sortBounds a 0 (length a)
{-# inlinable sort #-}
{-# specialize sort :: MVector s -> ST s () #-}
{-# specialize sort :: MVector (PrimState IO) -> IO () #-}

-- | Sorts the portion of an array delimited by [l,u)
sortBounds :: PrimMonad m => MVector (PrimState m) -> Int -> Int -> m ()
sortBounds !a !l !u
  | len < 2   = return ()
  | len == 2  = O.sort2ByOffset a l
  | len == 3  = O.sort3ByOffset a l
  | len == 4  = O.sort4ByOffset a l
  | otherwise = O.sort4ByOffset a l >> sortBounds' a l (l + 4) u
 where
 len = u - l
{-# inlinable sortBounds #-}
{-# specialize sortBounds :: MVector s -> Int -> Int -> ST s () #-}
{-# specialize
      sortBounds :: MVector (PrimState IO) -> Int -> Int -> IO () #-}

-- | Sorts the portion of the array delimited by [l,u) under the
-- assumption that [l,m) is already sorted.
sortBounds'
  :: PrimMonad m => MVector (PrimState m) -> Int -> Int -> Int -> m ()
sortBounds' !a !l !m !u = sort m
 where
 sort i
   | i < u     = do v <- read a i
                    insert a l v i
                    sort (i+1)
   | otherwise = return ()
{-# inlinable sortBounds' #-}
{-# specialize
      sortBounds' :: MVector s -> Int -> Int -> Int -> ST s () #-}
{-# specialize
      sortBounds' :: MVector (PrimState IO) -> Int -> Int -> Int -> IO ()
      #-}

-- Given a sorted array in [l,u), inserts val into its proper
-- position, yielding a sorted [l,u]
insert
  :: PrimMonad m => MVector (PrimState m) -> Int -> Elem -> Int -> m ()
insert !a !l = loop
 where
 loop !val j
   | j <= l    = write a l val
   | otherwise = do e <- read a (j - 1)
                    case compare val e of
                      LT -> write a j e >> loop val (j - 1)
                      _  -> write a j val
{-# inlinable insert #-}
{-# specialize
      insert :: MVector s -> Int -> Elem -> Int -> ST s () #-}
{-# specialize
      insert :: MVector (PrimState IO) -> Int -> Elem -> Int -> IO () #-}
