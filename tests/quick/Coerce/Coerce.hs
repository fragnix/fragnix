module Coerce where

import Data.Coerce (coerce)

newtype Elem a = Elem {getElem :: a}

instance Functor Elem where
    fmap = coerce 
