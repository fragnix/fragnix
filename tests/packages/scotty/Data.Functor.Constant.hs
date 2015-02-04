{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Functor/Constant.hs" #-}









































{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Constant
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The constant functor.
-----------------------------------------------------------------------------

module Data.Functor.Constant (
    Constant(..),
  ) where

import Data.Functor.Classes

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))

-- | Constant functor.
newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord)

-- These instances would be equivalent to the derived instances of the
-- newtype if the field were removed.

instance (Read a) => Read (Constant a b) where
    readsPrec = readsData $ readsUnary "Constant" Constant

instance (Show a) => Show (Constant a b) where
    showsPrec d (Constant x) = showsUnary "Constant" d x

instance (Eq a) => Eq1 (Constant a) where eq1 = (==)
instance (Ord a) => Ord1 (Constant a) where compare1 = compare
instance (Read a) => Read1 (Constant a) where readsPrec1 = readsPrec
instance (Show a) => Show1 (Constant a) where showsPrec1 = showsPrec

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
    traverse _ (Constant x) = pure (Constant x)

instance (Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant x <*> Constant y = Constant (x `mappend` y)
