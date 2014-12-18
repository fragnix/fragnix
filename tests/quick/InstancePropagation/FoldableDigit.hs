module FoldableDigit where

import Digit (Digit(..))

import Data.Foldable (Foldable(foldr,foldl,foldr1,foldl1))

instance Foldable Digit where
        foldr f z (One a) = a `f` z
        foldr f z (Two a b) = a `f` (b `f` z)
        foldr f z (Three a b c) = a `f` (b `f` (c `f` z))
        foldr f z (Four a b c d) = a `f` (b `f` (c `f` (d `f` z)))
        foldl f z (One a) = z `f` a
        foldl f z (Two a b) = (z `f` a) `f` b
        foldl f z (Three a b c) = ((z `f` a) `f` b) `f` c
        foldl f z (Four a b c d) = (((z `f` a) `f` b) `f` c) `f` d
        foldr1 _ (One a) = a
        foldr1 f (Two a b) = a `f` b
        foldr1 f (Three a b c) = a `f` (b `f` c)
        foldr1 f (Four a b c d) = a `f` (b `f` (c `f` d))
        foldl1 _ (One a) = a
        foldl1 f (Two a b) = a `f` b
        foldl1 f (Three a b c) = (a `f` b) `f` c
        foldl1 f (Four a b c d) = ((a `f` b) `f` c) `f` d
