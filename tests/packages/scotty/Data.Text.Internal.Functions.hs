{-# LINE 1 "Data/Text/Internal/Functions.hs" #-}
# 1 "Data/Text/Internal/Functions.hs"
# 1 "<command-line>"
# 10 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 10 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1


















































































































# 10 "<command-line>" 2
# 1 "Data/Text/Internal/Functions.hs"
{-# LANGUAGE CPP, DeriveDataTypeable #-}

-- |
-- Module      : Data.Text.Internal.Functions
-- Copyright   : 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Useful functions.

module Data.Text.Internal.Functions
    (
      intersperse
    ) where

-- | A lazier version of Data.List.intersperse.  The other version
-- causes space leaks!
intersperse :: a -> [a] -> [a]
intersperse _   []     = []
intersperse sep (x:xs) = x : go xs
  where
    go []     = []
    go (y:ys) = sep : y: go ys
{-# INLINE intersperse #-}
