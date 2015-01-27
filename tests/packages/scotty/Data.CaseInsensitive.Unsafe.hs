{-# LINE 1 "Data/CaseInsensitive/Unsafe.hs" #-}
# 1 "Data/CaseInsensitive/Unsafe.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1











































































































# 8 "<command-line>" 2
# 1 "Data/CaseInsensitive/Unsafe.hs"
{-# LANGUAGE CPP, NoImplicitPrelude #-}


{-# LANGUAGE Unsafe #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CaseInsensitive.Unsafe
-- Copyright   :  (c) 2011-2013 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Provides an unsafe way to create a case insensitive string-like value.
--
-----------------------------------------------------------------------------

module Data.CaseInsensitive.Unsafe   ( unsafeMk ) where
import Data.CaseInsensitive.Internal ( unsafeMk )
