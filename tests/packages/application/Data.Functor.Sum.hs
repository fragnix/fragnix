{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Functor/Sum.hs" #-}









































{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Sum
-- Copyright   :  (c) Ross Paterson 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Sums, lifted to functors.
-----------------------------------------------------------------------------

module Data.Functor.Sum (
    Sum(..),
  ) where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Classes
import Data.Monoid (mappend)
import Data.Traversable (Traversable(traverse))

-- | Lifted sum of functors.
data Sum f g a = InL (f a) | InR (g a)

instance (Eq1 f, Eq1 g, Eq a) => Eq (Sum f g a) where
    InL x1 == InL x2 = eq1 x1 x2
    InR y1 == InR y2 = eq1 y1 y2
    _ == _ = False

instance (Ord1 f, Ord1 g, Ord a) => Ord (Sum f g a) where
    compare (InL x1) (InL x2) = compare1 x1 x2
    compare (InL _) (InR _) = LT
    compare (InR _) (InL _) = GT
    compare (InR y1) (InR y2) = compare1 y1 y2

instance (Read1 f, Read1 g, Read a) => Read (Sum f g a) where
    readsPrec = readsData $
        readsUnary1 "InL" InL `mappend` readsUnary1 "InR" InR

instance (Show1 f, Show1 g, Show a) => Show (Sum f g a) where
    showsPrec d (InL x) = showsUnary1 "InL" d x
    showsPrec d (InR y) = showsUnary1 "InR" d y

instance (Eq1 f, Eq1 g) => Eq1 (Sum f g) where eq1 = (==)
instance (Ord1 f, Ord1 g) => Ord1 (Sum f g) where compare1 = compare
instance (Read1 f, Read1 g) => Read1 (Sum f g) where readsPrec1 = readsPrec
instance (Show1 f, Show1 g) => Show1 (Sum f g) where showsPrec1 = showsPrec

instance (Functor f, Functor g) => Functor (Sum f g) where
    fmap f (InL x) = InL (fmap f x)
    fmap f (InR y) = InR (fmap f y)

instance (Foldable f, Foldable g) => Foldable (Sum f g) where
    foldMap f (InL x) = foldMap f x
    foldMap f (InR y) = foldMap f y

instance (Traversable f, Traversable g) => Traversable (Sum f g) where
    traverse f (InL x) = InL <$> traverse f x
    traverse f (InR y) = InR <$> traverse f y
