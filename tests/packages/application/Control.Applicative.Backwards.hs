{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Control/Applicative/Backwards.hs" #-}









































{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Backwards
-- Copyright   :  (c) Russell O'Connor 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Making functors with an 'Applicative' instance that performs actions
-- in the reverse order.
-----------------------------------------------------------------------------

module Control.Applicative.Backwards (
    Backwards(..),
  ) where

import Data.Functor.Classes

import Prelude hiding (foldr, foldr1, foldl, foldl1)
import Control.Applicative
import Data.Foldable
import Data.Traversable

-- | The same functor, but with an 'Applicative' instance that performs
-- actions in the reverse order.
newtype Backwards f a = Backwards { forwards :: f a }

instance (Eq1 f, Eq a) => Eq (Backwards f a) where
    Backwards x == Backwards y = eq1 x y

instance (Ord1 f, Ord a) => Ord (Backwards f a) where
    compare (Backwards x) (Backwards y) = compare1 x y

instance (Read1 f, Read a) => Read (Backwards f a) where
    readsPrec = readsData $ readsUnary1 "Backwards" Backwards

instance (Show1 f, Show a) => Show (Backwards f a) where
    showsPrec d (Backwards x) = showsUnary1 "Backwards" d x

instance (Eq1 f) => Eq1 (Backwards f) where eq1 = (==)
instance (Ord1 f) => Ord1 (Backwards f) where compare1 = compare
instance (Read1 f) => Read1 (Backwards f) where readsPrec1 = readsPrec
instance (Show1 f) => Show1 (Backwards f) where showsPrec1 = showsPrec

-- | Derived instance.
instance (Functor f) => Functor (Backwards f) where
    fmap f (Backwards a) = Backwards (fmap f a)

-- | Apply @f@-actions in the reverse order.
instance (Applicative f) => Applicative (Backwards f) where
    pure a = Backwards (pure a)
    Backwards f <*> Backwards a = Backwards (a <**> f)

-- | Try alternatives in the same order as @f@.
instance (Alternative f) => Alternative (Backwards f) where
    empty = Backwards empty
    Backwards x <|> Backwards y = Backwards (x <|> y)

-- | Derived instance.
instance (Foldable f) => Foldable (Backwards f) where
    foldMap f (Backwards t) = foldMap f t
    foldr f z (Backwards t) = foldr f z t
    foldl f z (Backwards t) = foldl f z t
    foldr1 f (Backwards t) = foldl1 f t
    foldl1 f (Backwards t) = foldr1 f t

-- | Derived instance.
instance (Traversable f) => Traversable (Backwards f) where
    traverse f (Backwards t) = fmap Backwards (traverse f t)
    sequenceA (Backwards t) = fmap Backwards (sequenceA t)
