{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Control/Monad/Trans/Cont.hs" #-}











































{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Cont
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Continuation monads.
--
-- Delimited continuation operators are taken from Kenichi Asai and Oleg
-- Kiselyov's tutorial at CW 2011, \"Introduction to programming with
-- shift and reset\" (<http://okmij.org/ftp/continuations/#tutorial>).
--
-----------------------------------------------------------------------------

module Control.Monad.Trans.Cont (
    -- * The Cont monad
    Cont,
    cont,
    runCont,
    evalCont,
    mapCont,
    withCont,
    -- ** Delimited continuations
    reset, shift,
    -- * The ContT monad transformer
    ContT(..),
    evalContT,
    mapContT,
    withContT,
    callCC,
    -- ** Delimited continuations
    resetT, shiftT,
    -- * Lifting other operations
    liftLocal,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

import Control.Applicative

{- |
Continuation monad.
@Cont r a@ is a CPS computation that produces an intermediate result
of type @a@ within a CPS computation whose final result type is @r@.

The @return@ function simply creates a continuation which passes the value on.

The @>>=@ operator adds the bound function into the continuation chain.
-}
type Cont r = ContT r Identity

-- | Construct a continuation-passing computation from a function.
-- (The inverse of 'runCont')
cont :: ((a -> r) -> r) -> Cont r a
cont f = ContT (\ c -> Identity (f (runIdentity . c)))

-- | The result of running a CPS computation with a given final continuation.
-- (The inverse of 'cont')
runCont
    :: Cont r a         -- ^ continuation computation (@Cont@).
    -> (a -> r)         -- ^ the final continuation, which produces
                        -- the final result (often 'id').
    -> r
runCont m k = runIdentity (runContT m (Identity . k))

-- | The result of running a CPS computation with the identity as the
-- final continuation.
--
-- * @'evalCont' ('return' x) = x@
evalCont :: Cont r r -> r
evalCont m = runIdentity (evalContT m)

-- | Apply a function to transform the result of a continuation-passing
-- computation.
--
-- * @'runCont' ('mapCont' f m) = f . 'runCont' m@
mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f = mapContT (Identity . f . runIdentity)

-- | Apply a function to transform the continuation passed to a CPS
-- computation.
--
-- * @'runCont' ('withCont' f m) = 'runCont' m . f@
withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f = withContT ((Identity .) . f . (runIdentity .))

-- | @'reset' m@ delimits the continuation of any 'shift' inside @m@.
--
-- * @'reset' ('return' m) = 'return' m@
--
reset :: Cont r r -> Cont r' r
reset = resetT

-- | @'shift' f@ captures the continuation up to the nearest enclosing
-- 'reset' and passes it to @f@:
--
-- * @'reset' ('shift' f >>= k) = 'reset' (f ('evalCont' . k))@
--
shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = shiftT (f . (runIdentity .))

-- | The continuation monad transformer.
-- Can be used to add continuation handling to other monads.
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

-- | The result of running a CPS computation with 'return' as the
-- final continuation.
--
-- * @'evalContT' ('lift' m) = m@
evalContT :: (Monad m) => ContT r m r -> m r
evalContT m = runContT m return

-- | Apply a function to transform the result of a continuation-passing
-- computation.
--
-- * @'runContT' ('mapContT' f m) = f . 'runContT' m@
mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

-- | Apply a function to transform the continuation passed to a CPS
-- computation.
--
-- * @'runContT' ('withContT' f m) = 'runContT' m . f@
withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f

instance Functor (ContT r m) where
    fmap f m = ContT $ \ c -> runContT m (c . f)

instance Applicative (ContT r m) where
    pure x  = ContT ($ x)
    f <*> v = ContT $ \ c -> runContT f $ \ g -> runContT v (c . g)

instance Monad (ContT r m) where
    return x = ContT ($ x)
    m >>= k  = ContT $ \ c -> runContT m (\ x -> runContT (k x) c)

instance MonadTrans (ContT r) where
    lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
    liftIO = lift . liftIO

-- | @callCC@ (call-with-current-continuation) calls its argument
-- function, passing it the current continuation.  It provides
-- an escape continuation mechanism for use with continuation
-- monads.  Escape continuations one allow to abort the current
-- computation and return a value immediately.  They achieve
-- a similar effect to 'Control.Monad.Trans.Except.throwE'
-- and 'Control.Monad.Trans.Except.catchE' within an
-- 'Control.Monad.Trans.Except.ExceptT' monad.  The advantage of this
-- function over calling 'return' is that it makes the continuation
-- explicit, allowing more flexibility and better control.
--
-- The standard idiom used with @callCC@ is to provide a lambda-expression
-- to name the continuation. Then calling the named continuation anywhere
-- within its scope will escape from the computation, even if it is many
-- layers deep within nested computations.
callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c

-- | @'resetT' m@ delimits the continuation of any 'shiftT' inside @m@.
--
-- * @'resetT' ('lift' m) = 'lift' m@
--
resetT :: (Monad m) => ContT r m r -> ContT r' m r
resetT = lift . evalContT

-- | @'shiftT' f@ captures the continuation up to the nearest enclosing
-- 'resetT' and passes it to @f@:
--
-- * @'resetT' ('shiftT' f >>= k) = 'resetT' (f ('evalContT' . k))@
--
shiftT :: (Monad m) => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT f = ContT (evalContT . f)

-- | @'liftLocal' ask local@ yields a @local@ function for @'ContT' r m@.
liftLocal :: (Monad m) => m r' -> ((r' -> r') -> m r -> m r) ->
    (r' -> r') -> ContT r m a -> ContT r m a
liftLocal ask local f m = ContT $ \ c -> do
    r <- ask
    local f (runContT m (local (const r) . c))
