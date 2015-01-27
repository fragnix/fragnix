{-# LINE 1 "Control/Monad/STM.hs" #-}
# 1 "Control/Monad/STM.hs"
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
# 1 "Control/Monad/STM.hs"
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}


{-# LANGUAGE Trustworthy #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.STM
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- Software Transactional Memory: a modular composable concurrency
-- abstraction.  See
--
--  * /Composable memory transactions/, by Tim Harris, Simon Marlow, Simon
--    Peyton Jones, and Maurice Herlihy, in /ACM Conference on Principles
--    and Practice of Parallel Programming/ 2005.
--    <http://research.microsoft.com/Users/simonpj/papers/stm/index.htm>
--
-- This module only defines the 'STM' monad; you probably want to
-- import "Control.Concurrent.STM" (which exports "Control.Monad.STM").
-----------------------------------------------------------------------------

module Control.Monad.STM (
        STM,
        atomically,

        always,
        alwaysSucceeds,
        retry,
        orElse,
        check,

        throwSTM,
        catchSTM
  ) where







import GHC.Conc

import GHC.Exts
import Control.Monad.Fix












# 80 "Control/Monad/STM.hs"

check :: Bool -> STM ()
check b = if b then return () else retry


# 115 "Control/Monad/STM.hs"


data STMret a = STMret (State# RealWorld) a

liftSTM :: STM a -> State# RealWorld -> STMret a
liftSTM (STM m) = \s -> case m s of (# s', r #) -> STMret s' r

instance MonadFix STM where
  mfix k = STM $ \s ->
    let ans        = liftSTM (k r) s
        STMret _ r = ans
    in case ans of STMret s' x -> (# s', x #)
