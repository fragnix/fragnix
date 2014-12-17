module Using where

import Prelude hiding (foldl1)

import DataType (Digit)
import Class (Sized,size)
import Data.Foldable (foldl1)

instance Sized a => Sized (Digit a) where
        {-# INLINE size #-}
        size = foldl1 (+) . fmap size