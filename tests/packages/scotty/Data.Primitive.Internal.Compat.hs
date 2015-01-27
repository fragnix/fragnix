{-# LINE 1 "Data/Primitive/Internal/Compat.hs" #-}
# 1 "Data/Primitive/Internal/Compat.hs"
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
# 1 "Data/Primitive/Internal/Compat.hs"
{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : Data.Primitive.Internal.Compat
-- Copyright   : (c) Roman Leshchinskiy 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Compatibility functions
--

module Data.Primitive.Internal.Compat (
    isTrue#
  , mkNoRepType
  ) where


import Data.Data (mkNoRepType)





import GHC.Exts (isTrue#)








