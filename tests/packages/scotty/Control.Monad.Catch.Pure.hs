{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Control/Monad/Catch/Pure.hs" #-}













































{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Trustworthy #-}



--------------------------------------------------------------------
-- |
-- Copyright   :  (C) Edward Kmett 2013-2014, (c) Google Inc. 2012
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module supplies a \'pure\' monad transformer that can be used for
-- mock-testing code that throws exceptions, so long as those exceptions
-- are always thrown with 'throwM'.
--
-- Do not mix 'CatchT' with 'IO'. Choose one or the other for the
-- bottom of your transformer stack!
--------------------------------------------------------------------

module Control.Monad.Catch.Pure (
    -- * Transformer
    -- $transformer
    CatchT(..), Catch
  , runCatch
  , mapCatchT

  -- * Typeclass
  -- $mtl
  , module Control.Monad.Catch
  ) where

import Prelude hiding (foldr)

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader as Reader
import Control.Monad.RWS
import Data.Foldable
import Data.Functor.Identity
import Data.Traversable as Traversable

------------------------------------------------------------------------------
-- $mtl
-- The mtl style typeclass
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- $transformer
-- The @transformers@-style monad transfomer
------------------------------------------------------------------------------

-- | Add 'Exception' handling abilities to a 'Monad'.
--
-- This should /never/ be used in combination with 'IO'. Think of 'CatchT'
-- as an alternative base monad for use with mocking code that solely throws
-- exceptions via 'throwM'.
--
-- Note: that 'IO' monad has these abilities already, so stacking 'CatchT' on top
-- of it does not add any value and can possibly be confusing:
--
-- >>> (error "Hello!" :: IO ()) `catch` (\(e :: ErrorCall) -> liftIO $ print e)
-- Hello!
--
-- >>> runCatchT $ (error "Hello!" :: CatchT IO ()) `catch` (\(e :: ErrorCall) -> liftIO $ print e)
-- *** Exception: Hello!
--
-- >>> runCatchT $ (throwM (ErrorCall "Hello!") :: CatchT IO ()) `catch` (\(e :: ErrorCall) -> liftIO $ print e)
-- Hello!

newtype CatchT m a = CatchT { runCatchT :: m (Either SomeException a) }

type Catch = CatchT Identity

runCatch :: Catch a -> Either SomeException a
runCatch = runIdentity . runCatchT

instance Monad m => Functor (CatchT m) where
  fmap f (CatchT m) = CatchT (liftM (fmap f) m)

instance Monad m => Applicative (CatchT m) where
  pure a = CatchT (return (Right a))
  (<*>) = ap

instance Monad m => Monad (CatchT m) where
  return a = CatchT (return (Right a))
  CatchT m >>= k = CatchT $ m >>= \ea -> case ea of
    Left e -> return (Left e)
    Right a -> runCatchT (k a)
  fail = CatchT . return . Left . toException . userError

instance MonadFix m => MonadFix (CatchT m) where
  mfix f = CatchT $ mfix $ \a -> runCatchT $ f $ case a of
    Right r -> r
    _       -> error "empty mfix argument"

instance Foldable m => Foldable (CatchT m) where
  foldMap f (CatchT m) = foldMap (foldMapEither f) m where
    foldMapEither g (Right a) = g a
    foldMapEither _ (Left _) = mempty

instance (Monad m, Traversable m) => Traversable (CatchT m) where
  traverse f (CatchT m) = CatchT <$> Traversable.traverse (traverseEither f) m where
    traverseEither g (Right a) = Right <$> g a
    traverseEither _ (Left e) = pure (Left e)

instance Monad m => Alternative (CatchT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (CatchT m) where
  mzero = CatchT $ return $ Left $ toException $ userError ""
  mplus (CatchT m) (CatchT n) = CatchT $ m >>= \ea -> case ea of
    Left _ -> n
    Right a -> return (Right a)

instance MonadTrans CatchT where
  lift m = CatchT $ do
    a <- m
    return $ Right a

instance MonadIO m => MonadIO (CatchT m) where
  liftIO m = CatchT $ do
    a <- liftIO m
    return $ Right a

instance Monad m => MonadThrow (CatchT m) where
  throwM = CatchT . return . Left . toException
instance Monad m => MonadCatch (CatchT m) where
  catch (CatchT m) c = CatchT $ m >>= \ea -> case ea of
    Left e -> case fromException e of
      Just e' -> runCatchT (c e')
      Nothing -> return (Left e)
    Right a -> return (Right a)
-- | Note: This instance is only valid if the underlying monad has a single
-- exit point!
instance Monad m => MonadMask (CatchT m) where
  mask a = a id
  uninterruptibleMask a = a id

instance MonadState s m => MonadState s (CatchT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadReader e m => MonadReader e (CatchT m) where
  ask = lift ask
  local f (CatchT m) = CatchT (local f m)

instance MonadWriter w m => MonadWriter w (CatchT m) where
  tell = lift . tell
  listen = mapCatchT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a
  pass = mapCatchT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Left  l      -> (Left  l, id)
        Right (r, f) -> (Right r, f)
  writer aw = CatchT (Right `liftM` writer aw)

instance MonadRWS r w s m => MonadRWS r w s (CatchT m)

-- | Map the unwrapped computation using the given function.
--
-- @'runCatchT' ('mapCatchT' f m) = f ('runCatchT' m)@
mapCatchT :: (m (Either SomeException a) -> n (Either SomeException b))
          -> CatchT m a
          -> CatchT n b
mapCatchT f m = CatchT $ f (runCatchT m)
