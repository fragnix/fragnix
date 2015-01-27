{-# LINE 1 "Control/Concurrent/STM/TVar.hs" #-}
# 1 "Control/Concurrent/STM/TVar.hs"
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
# 1 "Control/Concurrent/STM/TVar.hs"
{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}


{-# LANGUAGE Trustworthy #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.TVar
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- TVar: Transactional variables
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.TVar (
        -- * TVars
        TVar,
        newTVar,
        newTVarIO,
        readTVar,
        readTVarIO,
        writeTVar,
        modifyTVar,
        modifyTVar',
        swapTVar,

        registerDelay,

        mkWeakTVar
  ) where


import GHC.Base
import GHC.Conc
import GHC.Weak




-- Like 'modifyIORef' but for 'TVar'.
-- | Mutate the contents of a 'TVar'. /N.B./, this version is
-- non-strict.
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
    x <- readTVar var
    writeTVar var (f x)
{-# INLINE modifyTVar #-}


-- | Strict version of 'modifyTVar'.
modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x
{-# INLINE modifyTVar' #-}


-- Like 'swapTMVar' but for 'TVar'.
-- | Swap the contents of a 'TVar' for a new value.
swapTVar :: TVar a -> a -> STM a
swapTVar var new = do
    old <- readTVar var
    writeTVar var new
    return old
{-# INLINE swapTVar #-}


-- | Make a 'Weak' pointer to a 'TVar', using the second argument as
-- a finalizer to run when 'TVar' is garbage-collected
--
-- @since 2.4.3
mkWeakTVar :: TVar a -> IO () -> IO (Weak (TVar a))
mkWeakTVar t@(TVar t#) f = IO $ \s ->
    case mkWeak# t# t f s of (# s1, w #) -> (# s1, Weak w #)
