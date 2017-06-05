{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Control/Monad/Trans/Select.hs" #-}














































{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AutoDeriveTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Select
-- Copyright   :  (c) Ross Paterson 2017
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Selection monad transformer, modelling search algorithms.
--
-- * Martin Escardo and Paulo Oliva.
--   "Selection functions, bar recursion and backward induction",
--   /Mathematical Structures in Computer Science/ 20:2 (2010), pp. 127-168.
--   <https://www.cs.bham.ac.uk/~mhe/papers/selection-escardo-oliva.pdf>
--
-- * Jules Hedges. "Monad transformers for backtracking search".
--   In /Proceedings of MSFP 2014/. <https://arxiv.org/abs/1406.2058>
-----------------------------------------------------------------------------

module Control.Monad.Trans.Select (
    -- * The Select monad
    Select,
    select,
    runSelect,
    -- * The SelectT monad transformer
    SelectT(SelectT),
    runSelectT,
    selectToCont,
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Functor.Identity

-- | Selection monad.
type Select r = SelectT r Identity

-- | Constructor for computations in the selection monad.
select :: ((a -> r) -> a) -> Select r a
select f = SelectT $ \ k -> Identity (f (runIdentity . k))
{-# INLINE select #-}

-- | Runs a @Select@ computation with a function for evaluating answers
-- to select a particular answer.  (The inverse of 'select'.)
runSelect :: Select r a -> (a -> r) -> a
runSelect m k = runIdentity (runSelectT m (Identity . k))
{-# INLINE runSelect #-}

-- | Selection monad transformer.
--
-- 'SelectT' is not a functor on the category of monads, and many operations
-- cannot be lifted through it.
newtype SelectT r m a = SelectT ((a -> m r) -> m a)

-- | Runs a @SelectT@ computation with a function for evaluating answers
-- to select a particular answer.  (The inverse of 'select'.)
runSelectT :: SelectT r m a -> (a -> m r) -> m a
runSelectT (SelectT g) = g
{-# INLINE runSelectT #-}

instance (Functor m) => Functor (SelectT r m) where
    fmap f (SelectT g) = SelectT (fmap f . g . (. f))
    {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (SelectT r m) where
    pure = lift . return
    {-# INLINE pure #-}
    SelectT gf <*> SelectT gx = SelectT $ \ k -> do
        let h f = liftM f (gx (k . f))
        f <- gf ((>>= k) . h)
        h f
    {-# INLINE (<*>) #-}

instance (Functor m, MonadPlus m) => Alternative (SelectT r m) where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (SelectT r m) where
    SelectT g >>= f = SelectT $ \ k -> do
        let h x = runSelectT (f x) k
        y <- g ((>>= k) . h)
        h y
    {-# INLINE (>>=) #-}

instance (Fail.MonadFail m) => Fail.MonadFail (SelectT r m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (SelectT r m) where
    mzero = SelectT (const mzero)
    {-# INLINE mzero #-}
    SelectT f `mplus` SelectT g = SelectT $ \ k -> f k `mplus` g k
    {-# INLINE mplus #-}

instance MonadTrans (SelectT r) where
    lift = SelectT . const
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (SelectT r m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

-- | Convert a selection computation to a continuation-passing computation.
selectToCont :: (Monad m) => SelectT r m a -> ContT r m a
selectToCont (SelectT g) = ContT $ \ k -> g k >>= k
{-# INLINE selectToCont #-}
