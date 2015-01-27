{-# LINE 1 "Data/CaseInsensitive.hs" #-}
# 1 "Data/CaseInsensitive.hs"
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
# 1 "Data/CaseInsensitive.hs"
{-# LANGUAGE CPP, NoImplicitPrelude #-}


{-# LANGUAGE Trustworthy #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.CaseInsensitive
-- Copyright   :  (c) 2011-2013 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module is intended to be imported qualified. May I suggest:
--
-- @
-- import           Data.CaseInsensitive  ( CI )
-- import qualified Data.CaseInsensitive as CI
-- @
--
-- /Note that the FoldCase instance for ByteStrings is only/
-- /guaranteed to be correct for ISO-8859-1 encoded strings!/
--
-----------------------------------------------------------------------------

module Data.CaseInsensitive ( CI
                            , mk
                            , original
                            , foldedCase
                            , map
                            , FoldCase(foldCase)
                            ) where
import Data.CaseInsensitive.Internal
