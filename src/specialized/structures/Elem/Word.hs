
module Elem.Word (Elem, compare) where

import qualified Prelude as P
import Data.Word (Word)

type Elem = Word

compare :: Elem -> Elem -> P.Ordering
compare = P.compare
{-# inline compare #-}
