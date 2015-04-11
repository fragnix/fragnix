{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Functor/Classes.hs" #-}











































{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Classes
-- Copyright   :  (c) Ross Paterson 2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Liftings of the Prelude classes 'Eq', 'Ord', 'Read' and 'Show' to
-- unary type constructors.
--
-- These classes are needed to express the constraints on arguments of
-- transformers in portable Haskell.  Thus for a new transformer @T@,
-- one might write instances like
--
-- > instance (Eq1 f) => Eq (T f a) where ...
-- > instance (Ord1 f) => Ord (T f a) where ...
-- > instance (Read1 f) => Read (T f a) where ...
-- > instance (Show1 f) => Show (T f a) where ...
--
-- If these instances can be defined, defining instances of the lifted
-- classes is mechanical:
--
-- > instance (Eq1 f) => Eq1 (T f) where eq1 = (==)
-- > instance (Ord1 f) => Ord1 (T f) where compare1 = compare
-- > instance (Read1 f) => Read1 (T f) where readsPrec1 = readsPrec
-- > instance (Show1 f) => Show1 (T f) where showsPrec1 = showsPrec
--
-----------------------------------------------------------------------------

module Data.Functor.Classes (
    -- * Liftings of Prelude classes
    Eq1(..),
    Ord1(..),
    Read1(..),
    Show1(..),
    -- * Helper functions
    -- $example
    readsData,
    readsUnary,
    readsUnary1,
    readsBinary1,
    showsUnary,
    showsUnary1,
    showsBinary1,
  ) where

import Control.Applicative (Const(Const))
import Data.Functor.Identity (Identity)

-- | Lifting of the 'Eq' class to unary type constructors.
class Eq1 f where
    eq1 :: (Eq a) => f a -> f a -> Bool

-- | Lifting of the 'Ord' class to unary type constructors.
class (Eq1 f) => Ord1 f where
    compare1 :: (Ord a) => f a -> f a -> Ordering

-- | Lifting of the 'Read' class to unary type constructors.
class Read1 f where
    readsPrec1 :: (Read a) => Int -> ReadS (f a)

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    showsPrec1 :: (Show a) => Int -> f a -> ShowS

-- Instances for Prelude type constructors

instance Eq1 Maybe where eq1 = (==)
instance Ord1 Maybe where compare1 = compare
instance Read1 Maybe where readsPrec1 = readsPrec
instance Show1 Maybe where showsPrec1 = showsPrec

instance Eq1 [] where eq1 = (==)
instance Ord1 [] where compare1 = compare
instance Read1 [] where readsPrec1 = readsPrec
instance Show1 [] where showsPrec1 = showsPrec

instance (Eq a) => Eq1 ((,) a) where eq1 = (==)
instance (Ord a) => Ord1 ((,) a) where compare1 = compare
instance (Read a) => Read1 ((,) a) where readsPrec1 = readsPrec
instance (Show a) => Show1 ((,) a) where showsPrec1 = showsPrec

instance (Eq a) => Eq1 (Either a) where eq1 = (==)
instance (Ord a) => Ord1 (Either a) where compare1 = compare
instance (Read a) => Read1 (Either a) where readsPrec1 = readsPrec
instance (Show a) => Show1 (Either a) where showsPrec1 = showsPrec

-- Instances for other functors defined in the base package

instance Eq1 Identity where eq1 = (==)
instance Ord1 Identity where compare1 = compare
instance Read1 Identity where readsPrec1 = readsPrec
instance Show1 Identity where showsPrec1 = showsPrec

instance (Eq a) => Eq1 (Const a) where
    eq1 (Const x) (Const y) = x == y
instance (Ord a) => Ord1 (Const a) where
    compare1 (Const x) (Const y) = compare x y
instance (Read a) => Read1 (Const a) where
    readsPrec1 = readsData $ readsUnary "Const" Const
instance (Show a) => Show1 (Const a) where
    showsPrec1 d (Const x) = showsUnary "Const" d x

-- Building blocks

-- | @'readsData' p d@ is a parser for datatypes where each alternative
-- begins with a data constructor.  It parses the constructor and
-- passes it to @p@.  Parsers for various constructors can be constructed
-- with 'readsUnary', 'readsUnary1' and 'readsBinary1', and combined with
-- @mappend@ from the @Monoid@ class.
readsData :: (String -> ReadS a) -> Int -> ReadS a
readsData reader d =
    readParen (d > 10) $ \ r -> [res | (kw,s) <- lex r, res <- reader kw s]

-- | @'readsUnary' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec'.
readsUnary :: (Read a) => String -> (a -> t) -> String -> ReadS t
readsUnary name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec 11 s]

-- | @'readsUnary1' n c n'@ matches the name of a unary data constructor
-- and then parses its argument using 'readsPrec1'.
readsUnary1 :: (Read1 f, Read a) => String -> (f a -> t) -> String -> ReadS t
readsUnary1 name cons kw s =
    [(cons x,t) | kw == name, (x,t) <- readsPrec1 11 s]

-- | @'readsBinary1' n c n'@ matches the name of a binary data constructor
-- and then parses its arguments using 'readsPrec1'.
readsBinary1 :: (Read1 f, Read1 g, Read a) =>
    String -> (f a -> g a -> t) -> String -> ReadS t
readsBinary1 name cons kw s =
    [(cons x y,u) | kw == name,
        (x,t) <- readsPrec1 11 s, (y,u) <- readsPrec1 11 t]

-- | @'showsUnary' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
showsUnary :: (Show a) => String -> Int -> a -> ShowS
showsUnary name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec 11 x

-- | @'showsUnary1' n d x@ produces the string representation of a unary data
-- constructor with name @n@ and argument @x@, in precedence context @d@.
showsUnary1 :: (Show1 f, Show a) => String -> Int -> f a -> ShowS
showsUnary1 name d x = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x

-- | @'showsBinary1' n d x@ produces the string representation of a binary
-- data constructor with name @n@ and arguments @x@ and @y@, in precedence
-- context @d@.
showsBinary1 :: (Show1 f, Show1 g, Show a) =>
    String -> Int -> f a -> g a -> ShowS
showsBinary1 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . showsPrec1 11 x .
        showChar ' ' . showsPrec1 11 y

{- $example
These functions can be used to assemble 'Read' and 'Show' instances for
new algebraic types.  For example, given the definition

> data T f a = Zero a | One (f a) | Two (f a) (f a)

a standard 'Read' instance may be defined as

> instance (Read1 f, Read a) => Read (T f a) where
>     readsPrec = readsData $
>         readsUnary "Zero" Zero `mappend`
>         readsUnary1 "One" One `mappend`
>         readsBinary1 "Two" Two

and the corresponding 'Show' instance as

> instance (Show1 f, Show a) => Show (T f a) where
>     showsPrec d (Zero x) = showsUnary "Zero" d x
>     showsPrec d (One x) = showsUnary1 "One" d x
>     showsPrec d (Two x y) = showsBinary1 "Two" d x y

-}
