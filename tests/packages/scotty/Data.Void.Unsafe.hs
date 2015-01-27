{-# LINE 1 "src/Data/Void/Unsafe.hs" #-}
# 1 "src/Data/Void/Unsafe.hs"
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
# 1 "src/Data/Void/Unsafe.hs"
{-# LANGUAGE CPP #-}


{-# LANGUAGE Unsafe #-}

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
module Data.Void.Unsafe
  ( unsafeVacuous
  , unsafeVacuousM
  ) where

import Data.Void


import Unsafe.Coerce


-- | If 'Void' is uninhabited than any 'Functor' that holds only values of the type 'Void'
-- is holding no values.
--
-- This is only safe for valid functors that do not perform GADT-like analysis on the argument.
unsafeVacuous :: Functor f => f Void -> f a

unsafeVacuous = unsafeCoerce




-- | If 'Void' is uninhabited then any 'Monad' that holds values of type 'Void'
-- is holding no values.
--
-- This is only safe for valid monads that do not perform GADT-like analysis on the argument.
unsafeVacuousM :: Monad m => m Void -> m a

unsafeVacuousM = unsafeCoerce
