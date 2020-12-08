
module MVector.Unboxed.Unsafe.Word
  ( MVector(..)
  , new
  , read
  , write
  , slice
  , copy
  , length
  ) where

import Prelude hiding (read, length)

import Elem.Word

import Control.Monad.Primitive (PrimMonad(PrimState))

import qualified Data.Vector.Unboxed.Mutable as U

newtype MVector s = MV (U.MVector s Word)

new :: PrimMonad m => Int -> m (MVector (PrimState m))
new i = MV <$> U.unsafeNew i
{-# inline new #-}

read :: PrimMonad m => MVector (PrimState m) -> Int -> m Elem
read (MV v) i = U.unsafeRead v i
{-# inline read #-}

write :: PrimMonad m => MVector (PrimState m) -> Int -> Elem -> m ()
write (MV v) i n = U.unsafeWrite v i n
{-# inline write #-}

slice :: Int -> Int -> MVector s -> MVector s
slice i j (MV v) = MV $ U.unsafeSlice i j v
{-# inline slice #-}

length :: MVector s -> Int
length (MV v) = U.length v
{-# inline length #-}

copy
  :: PrimMonad m => MVector (PrimState m) -> MVector (PrimState m) -> m ()
copy (MV v) (MV u) = U.unsafeCopy v u
{-# inline copy #-}

