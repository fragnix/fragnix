{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Control/Concurrent/Lifted.hs" #-}













































{-# LANGUAGE CPP, NoImplicitPrelude, FlexibleContexts, RankNTypes #-}

{-# LANGUAGE Trustworthy #-}

{- |
Module      :  Control.Concurrent.Lifted
Copyright   :  Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of "Control.Concurrent" with types generalized
from 'IO' to all monads in either 'MonadBase' or 'MonadBaseControl'.
-}

module Control.Concurrent.Lifted
    ( -- * Concurrent Haskell
      ThreadId

      -- * Basic concurrency operations
    , myThreadId
    , fork
    , forkWithUnmask
    , forkFinally
    , killThread
    , throwTo

      -- ** Threads with affinity
    , forkOn
    , forkOnWithUnmask
    , getNumCapabilities
    , setNumCapabilities
    , threadCapability

      -- * Scheduling
    , yield

      -- ** Blocking
      -- ** Waiting
    , threadDelay
    , threadWaitRead
    , threadWaitWrite

      -- * Communication abstractions
    , module Control.Concurrent.MVar.Lifted
    , module Control.Concurrent.Chan.Lifted
    , module Control.Concurrent.QSem.Lifted
    , module Control.Concurrent.QSemN.Lifted


      -- * Bound Threads
    , C.rtsSupportsBoundThreads
    , forkOS
    , isCurrentThreadBound
    , runInBoundThread
    , runInUnboundThread

      -- * Weak references to ThreadIds
    , mkWeakThreadId
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude            ( (.) )
import Data.Bool          ( Bool )
import Data.Int           ( Int )
import Data.Function      ( ($) )
import System.IO          ( IO )
import System.Posix.Types ( Fd )
import Control.Monad      ( (>>=) )
import Data.Either        ( Either )
import System.Mem.Weak    ( Weak )

import           Control.Concurrent ( ThreadId )
import qualified Control.Concurrent as C

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseOp_, liftBaseDiscard )

import Control.Monad.Trans.Control ( liftBaseWith )
import Control.Monad               ( void )

-- from lifted-base (this package):
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.QSem.Lifted
import Control.Concurrent.QSemN.Lifted
import Control.Exception.Lifted ( throwTo
                                , SomeException, try, mask
                                )


--------------------------------------------------------------------------------
-- Control.Concurrent
--------------------------------------------------------------------------------

-- | Generalized version of 'C.myThreadId'.
myThreadId :: MonadBase IO m => m ThreadId
myThreadId = liftBase C.myThreadId
{-# INLINE myThreadId #-}

-- | Generalized version of 'C.forkIO'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
fork :: MonadBaseControl IO m => m () -> m ThreadId
fork = liftBaseDiscard C.forkIO
{-# INLINE fork #-}

-- | Generalized version of 'C.forkIOWithUnmask'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkWithUnmask :: MonadBaseControl IO m => ((forall a. m a -> m a) -> m ()) -> m ThreadId
forkWithUnmask f = liftBaseWith $ \runInIO ->
                     C.forkIOWithUnmask $ \unmask ->
                       void $ runInIO $ f $ liftBaseOp_ unmask
{-# INLINE forkWithUnmask #-}

-- | Generalized version of 'C.forkFinally'.
--
-- Note that in @forkFinally action and_then@, while the forked
-- @action@ and the @and_then@ function have access to the captured
-- state, all their side-effects in @m@ are discarded. They're run
-- only for their side-effects in 'IO'.
forkFinally :: MonadBaseControl IO m
            => m a -> (Either SomeException a -> m ()) -> m ThreadId
forkFinally action and_then =
    mask $ \restore ->
      fork $ try (restore action) >>= and_then
{-# INLINE forkFinally #-}

-- | Generalized version of 'C.killThread'.
killThread :: MonadBase IO m => ThreadId -> m ()
killThread = liftBase . C.killThread
{-# INLINE  killThread #-}

-- | Generalized version of 'C.forkOn'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkOn :: MonadBaseControl IO m => Int -> m () -> m ThreadId
forkOn = liftBaseDiscard . C.forkOn
{-# INLINE forkOn #-}

-- | Generalized version of 'C.forkOnWithUnmask'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkOnWithUnmask :: MonadBaseControl IO m => Int -> ((forall a. m a -> m a) -> m ()) -> m ThreadId
forkOnWithUnmask cap f = liftBaseWith $ \runInIO ->
                           C.forkOnWithUnmask cap $ \unmask ->
                             void $ runInIO $ f $ liftBaseOp_ unmask
{-# INLINE forkOnWithUnmask #-}

-- | Generalized version of 'C.getNumCapabilities'.
getNumCapabilities :: MonadBase IO m => m Int
getNumCapabilities = liftBase C.getNumCapabilities
{-# INLINE getNumCapabilities #-}

-- | Generalized version of 'C.setNumCapabilities'.
setNumCapabilities :: MonadBase IO m => Int -> m ()
setNumCapabilities = liftBase . C.setNumCapabilities
{-# INLINE setNumCapabilities #-}

-- | Generalized version of 'C.threadCapability'.
threadCapability :: MonadBase IO m => ThreadId -> m (Int, Bool)
threadCapability = liftBase . C.threadCapability
{-# INLINE threadCapability #-}

-- | Generalized version of 'C.yield'.
yield :: MonadBase IO m => m ()
yield = liftBase C.yield
{-# INLINE yield #-}

-- | Generalized version of 'C.threadDelay'.
threadDelay :: MonadBase IO m => Int -> m ()
threadDelay = liftBase .  C.threadDelay
{-# INLINE threadDelay #-}

-- | Generalized version of 'C.threadWaitRead'.
threadWaitRead :: MonadBase IO m => Fd -> m ()
threadWaitRead = liftBase . C.threadWaitRead
{-# INLINE threadWaitRead #-}

-- | Generalized version of 'C.threadWaitWrite'.
threadWaitWrite :: MonadBase IO m => Fd -> m ()
threadWaitWrite = liftBase . C.threadWaitWrite
{-# INLINE threadWaitWrite #-}


-- | Generalized version of 'C.forkOS'.
--
-- Note that, while the forked computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in 'IO'.
forkOS :: MonadBaseControl IO m => m () -> m ThreadId
forkOS = liftBaseDiscard C.forkOS
{-# INLINE forkOS #-}

-- | Generalized version of 'C.isCurrentThreadBound'.
isCurrentThreadBound :: MonadBase IO m => m Bool
isCurrentThreadBound = liftBase C.isCurrentThreadBound
{-# INLINE isCurrentThreadBound #-}

-- | Generalized version of 'C.runInBoundThread'.
runInBoundThread :: MonadBaseControl IO m => m a -> m a
runInBoundThread = liftBaseOp_ C.runInBoundThread
{-# INLINE runInBoundThread #-}

-- | Generalized version of 'C.runInUnboundThread'.
runInUnboundThread :: MonadBaseControl IO m => m a -> m a
runInUnboundThread = liftBaseOp_ C.runInUnboundThread
{-# INLINE runInUnboundThread #-}

-- | Generalized versio  of 'C.mkWeakThreadId'.
mkWeakThreadId :: MonadBase IO m => ThreadId -> m (Weak ThreadId)
mkWeakThreadId = liftBase . C.mkWeakThreadId
{-# INLINE mkWeakThreadId #-}
