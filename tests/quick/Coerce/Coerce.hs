module Coerce where

import Data.Coerce (coerce)
import Data.Monoid (Monoid, (<>), mappend)

newtype Elem a = Elem {getElem :: a}

instance Functor Elem where
    fmap = coerce 

newtype WrappedMonoid m = WrapMonoid
  { unwrapMonoid :: m }

instance Functor WrappedMonoid where
    fmap = coerce 
