{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Control/Exception/Lifted.hs" #-}




















































{-# LANGUAGE CPP
           , NoImplicitPrelude
           , ExistentialQuantification
           , FlexibleContexts #-}

{-# LANGUAGE RankNTypes #-} -- for mask

{-# LANGUAGE Safe #-}

{- |
Module      :  Control.Exception.Lifted
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
Portability :  non-portable (extended exceptions)

This is a wrapped version of "Control.Exception" with types generalized
from 'IO' to all monads in either 'MonadBase' or 'MonadBaseControl'.
-}

module Control.Exception.Lifted
    ( module Control.Exception

      -- * Throwing exceptions
    , throwIO, ioError, throwTo

      -- * Catching exceptions
      -- ** The @catch@ functions
    , catch, catches, Handler(..), catchJust

      -- ** The @handle@ functions
    , handle, handleJust

      -- ** The @try@ functions
    , try, tryJust

      -- ** The @evaluate@ function
    , evaluate

      -- * Asynchronous Exceptions
      -- ** Asynchronous exception control
      -- |The following functions allow a thread to control delivery of
      -- asynchronous exceptions during a critical region.
    , mask, mask_
    , uninterruptibleMask, uninterruptibleMask_
    , getMaskingState
    , allowInterrupt

      -- * Brackets
    , bracket, bracket_, bracketOnError

      -- * Utilities
    , finally, onException
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude         ( (.) )
import Data.Function   ( ($) )
import Data.Either     ( Either(Left, Right), either )
import Data.Maybe      ( Maybe )
import Control.Monad   ( (>>=), return, liftM )
import System.IO.Error ( IOError )
import System.IO       ( IO )


import Control.Exception hiding
    ( throwIO, ioError, throwTo
    , catch, catches, Handler(..), catchJust
    , handle, handleJust
    , try, tryJust
    , evaluate
    , mask, mask_
    , uninterruptibleMask, uninterruptibleMask_
    , getMaskingState
    , allowInterrupt
    , bracket, bracket_, bracketOnError
    , finally, onException
    )
import qualified Control.Exception  as E
import qualified Control.Concurrent as C
import           Control.Concurrent ( ThreadId )


-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl, StM
                                   , liftBaseWith, restoreM
                                   , control, liftBaseOp_
                                   )


--------------------------------------------------------------------------------
-- * Throwing exceptions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.throwIO'.
throwIO :: (MonadBase IO m, Exception e) => e -> m a
throwIO = liftBase . E.throwIO
{-# INLINABLE throwIO #-}

-- |Generalized version of 'E.ioError'.
ioError :: MonadBase IO m => IOError -> m a
ioError = liftBase . E.ioError
{-# INLINABLE ioError #-}

-- | Generalized version of 'C.throwTo'.
throwTo :: (MonadBase IO m, Exception e) => ThreadId -> e -> m ()
throwTo tid e = liftBase $ C.throwTo tid e
{-# INLINABLE throwTo #-}

--------------------------------------------------------------------------------
-- * Catching exceptions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.catch'.
--
-- Note, when the given computation throws an exception any monadic
-- side effects in @m@ will be discarded.
catch :: (MonadBaseControl IO m, Exception e)
      => m a       -- ^ The computation to run
      -> (e -> m a) -- ^ Handler to invoke if an exception is raised
      -> m a
catch a handler = control $ \runInIO ->
                    E.catch (runInIO a)
                            (\e -> runInIO $ handler e)
{-# INLINABLE catch #-}

-- |Generalized version of 'E.catches'.
--
-- Note, when the given computation throws an exception any monadic
-- side effects in @m@ will be discarded.
catches :: MonadBaseControl IO m => m a -> [Handler m a] -> m a
catches a handlers = control $ \runInIO ->
                       E.catches (runInIO a)
                                 [ E.Handler $ \e -> runInIO $ handler e
                                 | Handler handler <- handlers
                                 ]
{-# INLINABLE catches #-}

-- |Generalized version of 'E.Handler'.
data Handler m a = forall e. Exception e => Handler (e -> m a)

-- |Generalized version of 'E.catchJust'.
--
-- Note, when the given computation throws an exception any monadic
-- side effects in @m@ will be discarded.
catchJust :: (MonadBaseControl IO m, Exception e)
          => (e -> Maybe b) -- ^ Predicate to select exceptions
          -> m a           -- ^ Computation to run
          -> (b -> m a)     -- ^ Handler
          -> m a
catchJust p a handler = control $ \runInIO ->
                          E.catchJust p
                                      (runInIO a)
                                      (\e -> runInIO (handler e))
{-# INLINABLE catchJust #-}


--------------------------------------------------------------------------------
--  ** The @handle@ functions
--------------------------------------------------------------------------------

-- |Generalized version of 'E.handle'.
--
-- Note, when the given computation throws an exception any monadic
-- side effects in @m@ will be discarded.
handle :: (MonadBaseControl IO m, Exception e) => (e -> m a) -> m a -> m a
handle handler a = control $ \runInIO ->
                     E.handle (\e -> runInIO (handler e))
                              (runInIO a)
{-# INLINABLE handle #-}

-- |Generalized version of 'E.handleJust'.
--
-- Note, when the given computation throws an exception any monadic
-- side effects in @m@ will be discarded.
handleJust :: (MonadBaseControl IO m, Exception e)
           => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJust p handler a = control $ \runInIO ->
                           E.handleJust p (\e -> runInIO (handler e))
                                          (runInIO a)
{-# INLINABLE handleJust #-}

--------------------------------------------------------------------------------
-- ** The @try@ functions
--------------------------------------------------------------------------------

sequenceEither :: MonadBaseControl IO m => Either e (StM m a) -> m (Either e a)
sequenceEither = either (return . Left) (liftM Right . restoreM)
{-# INLINE sequenceEither #-}

-- |Generalized version of 'E.try'.
--
-- Note, when the given computation throws an exception any monadic
-- side effects in @m@ will be discarded.
try :: (MonadBaseControl IO m, Exception e) => m a -> m (Either e a)
try m = liftBaseWith (\runInIO -> E.try (runInIO m)) >>= sequenceEither
{-# INLINABLE try #-}

-- |Generalized version of 'E.tryJust'.
--
-- Note, when the given computation throws an exception any monadic
-- side effects in @m@ will be discarded.
tryJust :: (MonadBaseControl IO m, Exception e) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust p m = liftBaseWith (\runInIO -> E.tryJust p (runInIO m)) >>= sequenceEither
{-# INLINABLE tryJust #-}


--------------------------------------------------------------------------------
-- ** The @evaluate@ function
--------------------------------------------------------------------------------

-- |Generalized version of 'E.evaluate'.
evaluate :: MonadBase IO m => a -> m a
evaluate = liftBase . E.evaluate
{-# INLINABLE evaluate #-}


--------------------------------------------------------------------------------
-- ** Asynchronous exception control
--------------------------------------------------------------------------------

-- |Generalized version of 'E.mask'.
mask :: MonadBaseControl IO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = control $ \runInBase ->
           E.mask $ \g -> runInBase $ f $ liftBaseOp_ g
{-# INLINABLE mask #-}

-- |Generalized version of 'E.mask_'.
mask_ :: MonadBaseControl IO m => m a -> m a
mask_ = liftBaseOp_ E.mask_
{-# INLINABLE mask_ #-}

-- |Generalized version of 'E.uninterruptibleMask'.
uninterruptibleMask
    :: MonadBaseControl IO m => ((forall a. m a -> m a) -> m b) -> m b
uninterruptibleMask f =
    control $ \runInBase ->
        E.uninterruptibleMask $ \g -> runInBase $ f $ liftBaseOp_ g

{-# INLINABLE uninterruptibleMask #-}

-- |Generalized version of 'E.uninterruptibleMask_'.
uninterruptibleMask_ :: MonadBaseControl IO m => m a -> m a
uninterruptibleMask_ = liftBaseOp_ E.uninterruptibleMask_
{-# INLINABLE uninterruptibleMask_ #-}

-- |Generalized version of 'E.getMaskingState'.
getMaskingState :: MonadBase IO m => m MaskingState
getMaskingState = liftBase E.getMaskingState
{-# INLINABLE getMaskingState #-}

-- |Generalized version of 'E.allowInterrupt'.
allowInterrupt :: MonadBase IO m => m ()
allowInterrupt = liftBase E.allowInterrupt
{-# INLINABLE allowInterrupt #-}



--------------------------------------------------------------------------------
-- * Brackets
--------------------------------------------------------------------------------

-- |Generalized version of 'E.bracket'.
--
-- Note:
--
-- * When the \"acquire\" or \"release\" computations throw exceptions
--   any monadic side effects in @m@ will be discarded.
--
-- * When the \"in-between\" computation throws an exception any
--   monadic side effects in @m@ produced by that computation will be
--   discarded but the side effects of the \"acquire\" or \"release\"
--   computations will be retained.
--
-- * Also, any monadic side effects in @m@ of the \"release\"
--   computation will be discarded; it is run only for its side
--   effects in @IO@.
--
-- Note that when your @acquire@ and @release@ computations are of type 'IO'
-- it will be more efficient to write:
--
-- @'liftBaseOp' ('E.bracket' acquire release)@
bracket :: MonadBaseControl IO m
        => m a       -- ^ computation to run first (\"acquire resource\")
        -> (a -> m b) -- ^ computation to run last (\"release resource\")
        -> (a -> m c) -- ^ computation to run in-between
        -> m c
bracket before after thing = control $ \runInIO ->
                               E.bracket (runInIO before)
                                         (\st -> runInIO $ restoreM st >>= after)
                                         (\st -> runInIO $ restoreM st >>= thing)
{-# INLINABLE bracket #-}

-- |Generalized version of 'E.bracket_'.
--
-- Note any monadic side effects in @m@ of /both/ the \"acquire\" and
-- \"release\" computations will be discarded. To keep the monadic
-- side effects of the \"acquire\" computation, use 'bracket' with
-- constant functions instead.
--
-- Note that when your @acquire@ and @release@ computations are of type 'IO'
-- it will be more efficient to write:
--
-- @'liftBaseOp_' ('E.bracket_' acquire release)@
bracket_ :: MonadBaseControl IO m
         => m a -- ^ computation to run first (\"acquire resource\")
         -> m b -- ^ computation to run last (\"release resource\")
         -> m c -- ^ computation to run in-between
         -> m c
bracket_ before after thing = control $ \runInIO ->
                                E.bracket_ (runInIO before)
                                           (runInIO after)
                                           (runInIO thing)
{-# INLINABLE bracket_ #-}

-- |Generalized version of 'E.bracketOnError'.
--
-- Note:
--
-- * When the \"acquire\" or \"release\" computations throw exceptions
--   any monadic side effects in @m@ will be discarded.
--
-- * When the \"in-between\" computation throws an exception any
--   monadic side effects in @m@ produced by that computation will be
--   discarded but the side effects of the \"acquire\" computation
--   will be retained.
--
-- * Also, any monadic side effects in @m@ of the \"release\"
--   computation will be discarded; it is run only for its side
--   effects in @IO@.
--
-- Note that when your @acquire@ and @release@ computations are of
-- type 'IO' it will be more efficient to write:
--
-- @'liftBaseOp' ('E.bracketOnError' acquire release)@
bracketOnError :: MonadBaseControl IO m
               => m a       -- ^ computation to run first (\"acquire resource\")
               -> (a -> m b) -- ^ computation to run last (\"release resource\")
               -> (a -> m c) -- ^ computation to run in-between
               -> m c
bracketOnError before after thing =
    control $ \runInIO ->
      E.bracketOnError (runInIO before)
                       (\st -> runInIO $ restoreM st >>= after)
                       (\st -> runInIO $ restoreM st >>= thing)
{-# INLINABLE bracketOnError #-}


--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- |Generalized version of 'E.finally'.
--
-- Note, any monadic side effects in @m@ of the \"afterward\"
-- computation will be discarded.
finally :: MonadBaseControl IO m
        => m a -- ^ computation to run first
        -> m b -- ^ computation to run afterward (even if an exception was raised)
        -> m a
finally a sequel = control $ \runInIO ->
                     E.finally (runInIO a)
                               (runInIO sequel)
{-# INLINABLE finally #-}

-- |Generalized version of 'E.onException'.
--
-- Note, any monadic side effects in @m@ of the \"afterward\"
-- computation will be discarded.
onException :: MonadBaseControl IO m => m a -> m b -> m a
onException m what = control $ \runInIO ->
                       E.onException (runInIO m)
                                     (runInIO what)
{-# INLINABLE onException #-}
