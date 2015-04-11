{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Functor/Compose.hs" #-}











































{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Compose
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Composition of functors.
-----------------------------------------------------------------------------

module Data.Functor.Compose (
    Compose(..),
  ) where

import Data.Functor.Classes

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

infixr 9 `Compose`

-- | Right-to-left composition of functors.
-- The composition of applicative functors is always applicative,
-- but the composition of monads is not always a monad.
newtype Compose f g a = Compose { getCompose :: f (g a) }

-- Instances of Prelude classes

-- kludge to get type with the same instances as g a
newtype Apply g a = Apply (g a)

getApply :: Apply g a -> g a
getApply (Apply x) = x

instance (Eq1 g, Eq a) => Eq (Apply g a) where
    Apply x == Apply y = eq1 x y

instance (Ord1 g, Ord a) => Ord (Apply g a) where
    compare (Apply x) (Apply y) = compare1 x y

instance (Read1 g, Read a) => Read (Apply g a) where
    readsPrec d s = [(Apply a, t) | (a, t) <- readsPrec1 d s]

instance (Show1 g, Show a) => Show (Apply g a) where
    showsPrec d (Apply x) = showsPrec1 d x

instance (Functor f, Eq1 f, Eq1 g, Eq a) => Eq (Compose f g a) where
    Compose x == Compose y = eq1 (fmap Apply x) (fmap Apply y)

instance (Functor f, Ord1 f, Ord1 g, Ord a) => Ord (Compose f g a) where
    compare (Compose x) (Compose y) = compare1 (fmap Apply x) (fmap Apply y)

instance (Functor f, Read1 f, Read1 g, Read a) => Read (Compose f g a) where
    readsPrec = readsData $ readsUnary1 "Compose" (Compose . fmap getApply)

instance (Functor f, Show1 f, Show1 g, Show a) => Show (Compose f g a) where
    showsPrec d (Compose x) = showsUnary1 "Compose" d (fmap Apply x)

instance (Functor f, Eq1 f, Eq1 g) => Eq1 (Compose f g) where eq1 = (==)
instance (Functor f, Ord1 f, Ord1 g) => Ord1 (Compose f g) where
    compare1 = compare
instance (Functor f, Read1 f, Read1 g) => Read1 (Compose f g) where
    readsPrec1 = readsPrec
instance (Functor f, Show1 f, Show1 g) => Show1 (Compose f g) where
    showsPrec1 = showsPrec

-- Functor instances

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
    empty = Compose empty
    Compose x <|> Compose y = Compose (x <|> y)
