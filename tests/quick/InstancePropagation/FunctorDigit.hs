module FunctorDigit where

import Digit (Digit(..))

instance Functor Digit where
        {-# INLINE fmap #-}
        fmap f (One a) = One (f a)
        fmap f (Two a b) = Two (f a) (f b)
        fmap f (Three a b c) = Three (f a) (f b) (f c)
        fmap f (Four a b c d) = Four (f a) (f b) (f c) (f d)