{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/Primitive/Internal/Compat.hs" #-}











































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




