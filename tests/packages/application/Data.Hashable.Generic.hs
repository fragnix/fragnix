{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Hashable/Generic.hs" #-}
{-# LANGUAGE BangPatterns, FlexibleInstances, KindSignatures,
             ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------------------------
-- |
-- Module      :  Data.Hashable.Generic
-- Copyright   :  (c) Bryan O'Sullivan 2012
-- License     :  BSD-style
-- Maintainer  :  bos@serpentine.com
-- Stability   :  provisional
-- Portability :  GHC >= 7.2
--
-- Hashable support for GHC generics.

module Data.Hashable.Generic
    (
    ) where

import Data.Bits (Bits, shiftR)
import Data.Hashable.Class
import GHC.Generics

-- Type without constructors
instance GHashable V1 where
    ghashWithSalt salt _ = hashWithSalt salt ()

-- Constructor without arguments
instance GHashable U1 where
    ghashWithSalt salt U1 = hashWithSalt salt ()

instance (GHashable a, GHashable b) => GHashable (a :*: b) where
    ghashWithSalt salt (x :*: y) = salt `ghashWithSalt` x `ghashWithSalt` y

-- Metadata (constructor name, etc)
instance GHashable a => GHashable (M1 i c a) where
    ghashWithSalt salt = ghashWithSalt salt . unM1

-- Constants, additional parameters, and rank-1 recursion
instance Hashable a => GHashable (K1 i a) where
    ghashWithSalt = hashUsing unK1

class GSum f where
    hashSum :: Int -> Int -> Int -> f a -> Int

instance (GSum a, GSum b, GHashable a, GHashable b,
          SumSize a, SumSize b) => GHashable (a :+: b) where
    ghashWithSalt salt = hashSum salt 0 size
        where size = unTagged (sumSize :: Tagged (a :+: b))

instance (GSum a, GSum b, GHashable a, GHashable b) => GSum (a :+: b) where
    hashSum !salt !code !size s = case s of
                                    L1 x -> hashSum salt code           sizeL x
                                    R1 x -> hashSum salt (code + sizeL) sizeR x
        where
          sizeL = size `shiftR` 1
          sizeR = size - sizeL
    {-# INLINE hashSum #-}

instance GHashable a => GSum (C1 c a) where
    hashSum !salt !code _ x = salt `hashWithSalt` code `ghashWithSalt` x
    {-# INLINE hashSum #-}

class SumSize f where
    sumSize :: Tagged f

newtype Tagged (s :: * -> *) = Tagged {unTagged :: Int}

instance (SumSize a, SumSize b) => SumSize (a :+: b) where
    sumSize = Tagged $ unTagged (sumSize :: Tagged a) +
                       unTagged (sumSize :: Tagged b)

instance SumSize (C1 c a) where
    sumSize = Tagged 1
