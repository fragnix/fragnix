{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Functor/Reverse.hs" #-}











































{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Reverse
-- Copyright   :  (c) Russell O'Connor 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Making functors whose elements are notionally in the reverse order
-- from the original functor.
-----------------------------------------------------------------------------

module Data.Functor.Reverse (
    Reverse(..),
  ) where

import Control.Applicative.Backwards
import Data.Functor.Classes

import Prelude hiding (foldr, foldr1, foldl, foldl1)
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

-- | The same functor, but with 'Foldable' and 'Traversable' instances
-- that process the elements in the reverse order.
newtype Reverse f a = Reverse { getReverse :: f a }

instance (Eq1 f, Eq a) => Eq (Reverse f a) where
    Reverse x == Reverse y = eq1 x y

instance (Ord1 f, Ord a) => Ord (Reverse f a) where
    compare (Reverse x) (Reverse y) = compare1 x y

instance (Read1 f, Read a) => Read (Reverse f a) where
    readsPrec = readsData $ readsUnary1 "Reverse" Reverse

instance (Show1 f, Show a) => Show (Reverse f a) where
    showsPrec d (Reverse x) = showsUnary1 "Reverse" d x

instance (Eq1 f) => Eq1 (Reverse f) where eq1 = (==)
instance (Ord1 f) => Ord1 (Reverse f) where compare1 = compare
instance (Read1 f) => Read1 (Reverse f) where readsPrec1 = readsPrec
instance (Show1 f) => Show1 (Reverse f) where showsPrec1 = showsPrec

-- | Derived instance.
instance (Functor f) => Functor (Reverse f) where
    fmap f (Reverse a) = Reverse (fmap f a)

-- | Derived instance.
instance (Applicative f) => Applicative (Reverse f) where
    pure a = Reverse (pure a)
    Reverse f <*> Reverse a = Reverse (f <*> a)

-- | Derived instance.
instance (Alternative f) => Alternative (Reverse f) where
    empty = Reverse empty
    Reverse x <|> Reverse y = Reverse (x <|> y)

-- | Fold from right to left.
instance (Foldable f) => Foldable (Reverse f) where
    foldMap f (Reverse t) = getDual (foldMap (Dual . f) t)
    foldr f z (Reverse t) = foldl (flip f) z t
    foldl f z (Reverse t) = foldr (flip f) z t
    foldr1 f (Reverse t) = foldl1 (flip f) t
    foldl1 f (Reverse t) = foldr1 (flip f) t

-- | Traverse from right to left.
instance (Traversable f) => Traversable (Reverse f) where
    traverse f (Reverse t) =
        fmap Reverse . forwards $ traverse (Backwards . f) t
    sequenceA (Reverse t) =
        fmap Reverse . forwards $ sequenceA (fmap Backwards t)
