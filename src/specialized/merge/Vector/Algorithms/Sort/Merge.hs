{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Vector.Algorithms.Sort.Merge
-- Copyright   : (c) 2008-2020 Dan Doel
-- Maintainer  : Dan Doel <dan.doel@gmail.com>
-- Stability   : Experimental
--
-- This module implements a simple top-down merge sort. The temporary
-- buffer is preallocated to 1/2 the size of the input array, and
-- shared through the entire sorting process to ease the amount of
-- allocation performed in total. This is a stable sort.

module Vector.Algorithms.Sort.Merge (sort) where

import Prelude hiding (read, length, compare)

import Control.Monad.ST (ST)
import Control.Monad.Primitive (PrimMonad(PrimState))

import Data.Bits

import Elem
import MVector

import qualified Vector.Algorithms.Sort.Insertion as I

-- | Sorts an entire array
sort :: PrimMonad m => MVector (PrimState m) -> m ()
sort vec
  | len < threshold = I.sort vec
  | otherwise = new halfLen >>= mergeSortWithBuf vec
  where
  len = length vec
  -- odd lengths have a larger half that needs to fit, so use ceiling,
  -- not floor
  halfLen = (len + 1) `div` 2
{-# inlinable sort #-}
{-# specialize sort :: MVector s -> ST s () #-}
{-# specialize sort :: MVector (PrimState IO) -> IO () #-}

mergeSortWithBuf
  :: PrimMonad m => MVector (PrimState m) -> MVector (PrimState m) -> m ()
mergeSortWithBuf !src !buf = loop 0 (length src)
 where
 loop !l !u
   | len < threshold = I.sortBounds src l u
   | otherwise = do
     loop l mid
     loop mid u
     merge (slice l len src) buf (mid - l)
  where len = u - l
        mid = midPoint u l
{-# inlinable mergeSortWithBuf #-}
{-# specialize mergeSortWithBuf :: MVector s -> MVector s -> ST s () #-}
{-# specialize
     mergeSortWithBuf
       :: MVector (PrimState IO) -> MVector (PrimState IO) -> IO () #-}

merge
  :: PrimMonad m
  => MVector (PrimState m) -> MVector (PrimState m) -> Int -> m ()
merge !src !buf !mid = do
  copy low lower
  eTmp <- read low 0
  eUpp <- read high 0
  loop 0 eTmp 0 eUpp 0
 where
 lower = slice 0   mid                src
 high  = slice mid (length src - mid) src
 low   = slice 0   mid                buf

 wroteHigh !iLow !eLow !iHigh !iIns
   | iHigh >= length high = copy (slice iIns (length low - iLow) src)
                                 (slice iLow (length low - iLow) low)
   | otherwise            = do eHigh <- read high iHigh
                               loop iLow eLow iHigh eHigh iIns

 wroteLow !iLow !iHigh !eHigh !iIns
   | iLow  >= length low  = return ()
   | otherwise            = do eLow <- read low iLow
                               loop iLow eLow iHigh eHigh iIns

 loop !iLow !eLow !iHigh !eHigh !iIns
   | LT <- compare eHigh eLow = do
     write src iIns eHigh
     wroteHigh iLow eLow (iHigh + 1) (iIns + 1)
   | otherwise = do
     write src iIns eLow
     wroteLow (iLow + 1) iHigh eHigh (iIns + 1)
{-# inlinable merge #-}
{-# specialize
      merge :: MVector s -> MVector s -> Int -> ST s () #-}
{-# specialize
      merge :: MVector (PrimState IO)
            -> MVector (PrimState IO)
            -> Int
            -> IO () #-}

midPoint :: Int -> Int -> Int
midPoint a b = (a + b) `shiftR` 1
{-# inline midPoint #-}

threshold :: Int
threshold = 25
{-# inline threshold #-}
