{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/Proxy/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Proxy.Compat (
  module Base,
  asProxyTypeOf
) where

import Data.Proxy as Base hiding (asProxyTypeOf)

import Prelude (const)

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}
