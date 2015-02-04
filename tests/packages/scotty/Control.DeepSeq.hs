{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Control/DeepSeq.hs" #-}











































{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.DeepSeq
-- Copyright   :  (c) The University of Glasgow 2001-2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides an overloaded function, 'deepseq', for fully
-- evaluating data structures (that is, evaluating to \"Normal Form\").
--
-- A typical use is to prevent resource leaks in lazy IO programs, by
-- forcing all characters from a file to be read. For example:
--
-- > import System.IO
-- > import Control.DeepSeq
-- >
-- > main = do
-- >     h <- openFile "f" ReadMode
-- >     s <- hGetContents h
-- >     s `deepseq` hClose h
-- >     return s
--
-- 'deepseq' differs from 'seq' as it traverses data structures deeply,
-- for example, 'seq' will evaluate only to the first constructor in
-- the list:
--
-- > > [1,2,undefined] `seq` 3
-- > 3
--
-- While 'deepseq' will force evaluation of all the list elements:
--
-- > > [1,2,undefined] `deepseq` 3
-- > *** Exception: Prelude.undefined
--
-- Another common use is to ensure any exceptions hidden within lazy
-- fields of a data structure do not leak outside the scope of the
-- exception handler, or to force evaluation of a data structure in one
-- thread, before passing to another thread (preventing work moving to
-- the wrong threads).
--
-- /Since: 1.1.0.0/
module Control.DeepSeq (
     deepseq, ($!!), force,
     NFData(..),
  ) where

import Data.Int
import Data.Word
import Data.Ratio
import Data.Complex
import Data.Array
import Data.Fixed
import Data.Version

infixr 0 $!!

-- | 'deepseq': fully evaluates the first argument, before returning the
-- second.
--
-- The name 'deepseq' is used to illustrate the relationship to 'seq':
-- where 'seq' is shallow in the sense that it only evaluates the top
-- level of its argument, 'deepseq' traverses the entire data structure
-- evaluating it completely.
--
-- 'deepseq' can be useful for forcing pending exceptions,
-- eradicating space leaks, or forcing lazy I/O to happen.  It is
-- also useful in conjunction with parallel Strategies (see the
-- @parallel@ package).
--
-- There is no guarantee about the ordering of evaluation.  The
-- implementation may evaluate the components of the structure in
-- any order or in parallel.  To impose an actual order on
-- evaluation, use 'pseq' from "Control.Parallel" in the
-- @parallel@ package.
--
-- /Since: 1.1.0.0/
deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b

-- | the deep analogue of '$!'.  In the expression @f $!! x@, @x@ is
-- fully evaluated before the function @f@ is applied to it.
--
-- /Since: 1.2.0.0/
($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x

-- | a variant of 'deepseq' that is useful in some circumstances:
--
-- > force x = x `deepseq` x
--
-- @force x@ fully evaluates @x@, and then returns it.  Note that
-- @force x@ only performs evaluation when the value of @force x@
-- itself is demanded, so essentially it turns shallow evaluation into
-- deep evaluation.
--
-- /Since: 1.2.0.0/
force :: (NFData a) => a -> a
force x = x `deepseq` x

-- | A class of types that can be fully evaluated.
--
-- /Since: 1.1.0.0/
class NFData a where
    -- | rnf should reduce its argument to normal form (that is, fully
    -- evaluate all sub-components), and then return '()'.
    --
    -- The default implementation of 'rnf' is
    --
    -- > rnf a = a `seq` ()
    --
    -- which may be convenient when defining instances for data types with
    -- no unevaluated fields (e.g. enumerations).
    rnf :: a -> ()
    rnf a = a `seq` ()

instance NFData Int
instance NFData Word
instance NFData Integer
instance NFData Float
instance NFData Double

instance NFData Char
instance NFData Bool
instance NFData ()

instance NFData Int8
instance NFData Int16
instance NFData Int32
instance NFData Int64

instance NFData Word8
instance NFData Word16
instance NFData Word32
instance NFData Word64

-- |/Since: 1.3.0.0/
instance NFData (Fixed a)

-- |This instance is for convenience and consistency with 'seq'.
-- This assumes that WHNF is equivalent to NF for functions.
--
-- /Since: 1.3.0.0/
instance NFData (a -> b)

--Rational and complex numbers.

instance (Integral a, NFData a) => NFData (Ratio a) where
  rnf x = rnf (numerator x, denominator x)

instance (RealFloat a, NFData a) => NFData (Complex a) where
  rnf (x:+y) = rnf x `seq`
               rnf y `seq`
               ()

instance NFData a => NFData (Maybe a) where
    rnf Nothing  = ()
    rnf (Just x) = rnf x

instance (NFData a, NFData b) => NFData (Either a b) where
    rnf (Left x)  = rnf x
    rnf (Right y) = rnf y

-- |/Since: 1.3.0.0/
instance NFData Data.Version.Version where
    rnf (Data.Version.Version branch tags) = rnf branch `seq` rnf tags

instance NFData a => NFData [a] where
    rnf [] = ()
    rnf (x:xs) = rnf x `seq` rnf xs

instance (Ix a, NFData a, NFData b) => NFData (Array a b) where
    rnf x = rnf (bounds x, Data.Array.elems x)

instance (NFData a, NFData b) => NFData (a,b) where
  rnf (x,y) = rnf x `seq` rnf y

instance (NFData a, NFData b, NFData c) => NFData (a,b,c) where
  rnf (x,y,z) = rnf x `seq` rnf y `seq` rnf z

instance (NFData a, NFData b, NFData c, NFData d) => NFData (a,b,c,d) where
  rnf (x1,x2,x3,x4) = rnf x1 `seq`
                      rnf x2 `seq`
                      rnf x3 `seq`
                      rnf x4

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5) =>
         NFData (a1, a2, a3, a4, a5) where
  rnf (x1, x2, x3, x4, x5) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6) =>
         NFData (a1, a2, a3, a4, a5, a6) where
  rnf (x1, x2, x3, x4, x5, x6) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5 `seq`
                  rnf x6

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7) =>
         NFData (a1, a2, a3, a4, a5, a6, a7) where
  rnf (x1, x2, x3, x4, x5, x6, x7) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5 `seq`
                  rnf x6 `seq`
                  rnf x7

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8) =>
         NFData (a1, a2, a3, a4, a5, a6, a7, a8) where
  rnf (x1, x2, x3, x4, x5, x6, x7, x8) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5 `seq`
                  rnf x6 `seq`
                  rnf x7 `seq`
                  rnf x8

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8, NFData a9) =>
         NFData (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  rnf (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
                  rnf x1 `seq`
                  rnf x2 `seq`
                  rnf x3 `seq`
                  rnf x4 `seq`
                  rnf x5 `seq`
                  rnf x6 `seq`
                  rnf x7 `seq`
                  rnf x8 `seq`
                  rnf x9
