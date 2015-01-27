{-# LINE 1 "src-old/Data/Void.hs" #-}















































{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Void
  ( Void
  , absurd
  , vacuous
  , vacuousM
  ) where

import Control.Monad (liftM)
import Data.Ix
import Data.Hashable
import Data.Semigroup (Semigroup(..))

import Data.Data

import GHC.Generics

import Control.Exception

-- | A logically uninhabited data type.
newtype Void = Void Void
  deriving (Data, Typeable)

deriving instance Generic Void

instance Eq Void where
  _ == _ = True

instance Hashable Void where
  hashWithSalt _ = absurd

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  showsPrec _ = absurd

-- | Reading a 'Void' value is always a parse error, considering 'Void' as
-- a data type with no constructors.
instance Read Void where
  readsPrec _ _ = []

-- | Since 'Void' values logically don't exist, this witnesses the logical
-- reasoning tool of \"ex falso quodlibet\".
absurd :: Void -> a
absurd a = a `seq` spin a where
   spin (Void b) = spin b

-- | If 'Void' is uninhabited then any 'Functor' that holds only values of type 'Void'
-- is holding no values.
vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd

-- | If 'Void' is uninhabited then any 'Monad' that holds values of type 'Void'
-- is holding no values.
vacuousM :: Monad m => m Void -> m a
vacuousM = liftM absurd

instance Semigroup Void where
  a <> _ = a
  times1p _ a = a

instance Ix Void where
  range _ = []
  index _ = absurd
  inRange _ = absurd
  rangeSize _ = 0

instance Exception Void
