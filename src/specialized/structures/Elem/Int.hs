
module Elem.Int (Elem, compare) where

import qualified Prelude as P

type Elem = P.Int

compare :: Elem -> Elem -> P.Ordering
compare = P.compare
{-# inline compare #-}
