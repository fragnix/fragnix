{-# LINE 1 "./Control/Monad/Trans/Maybe.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Control/Monad/Trans/Maybe.hs" #-}
{-# LINE 1 "./Control/Monad/Trans/Maybe.hs" #-}
{-# LANGUAGE CPP #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Maybe
-- Copyright   :  (c) 2007 Yitzak Gale, Eric Kidd
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'MaybeT' monad transformer extends a monad with the ability to exit
-- the computation without returning a value.
--
-- A sequence of actions produces a value only if all the actions in
-- the sequence do.  If one exits, the rest of the sequence is skipped
-- and the composite action exits.
--
-- For a variant allowing a range of exception values, see
-- "Control.Monad.Trans.Except".
-----------------------------------------------------------------------------

module Control.Monad.Trans.Maybe (
    -- * The MaybeT monad transformer
    MaybeT(..),
    mapMaybeT,
    -- * Conversion
    maybeToExceptT,
    exceptToMaybeT,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
    liftListen,
    liftPass,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT(..))
import Data.Functor.Classes

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus), liftM, ap)
import Control.Monad.Fix (MonadFix(mfix))
import Data.Foldable (Foldable(foldMap))
import Data.Maybe (fromMaybe)
import Data.Traversable (Traversable(traverse))

-- | The parameterizable maybe monad, obtained by composing an arbitrary
-- monad with the 'Maybe' monad.
--
-- Computations are actions that may produce a value or exit.
--
-- The 'return' function yields a computation that produces that
-- value, while @>>=@ sequences two subcomputations, exiting if either
-- computation does.
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Eq1 m, Eq a) => Eq (MaybeT m a) where
    MaybeT x == MaybeT y = eq1 x y

instance (Ord1 m, Ord a) => Ord (MaybeT m a) where
    compare (MaybeT x) (MaybeT y) = compare1 x y

instance (Read1 m, Read a) => Read (MaybeT m a) where
    readsPrec = readsData $ readsUnary1 "MaybeT" MaybeT

instance (Show1 m, Show a) => Show (MaybeT m a) where
    showsPrec d (MaybeT m) = showsUnary1 "MaybeT" d m

instance (Eq1 m) => Eq1 (MaybeT m) where eq1 = (==)
instance (Ord1 m) => Ord1 (MaybeT m) where compare1 = compare
instance (Read1 m) => Read1 (MaybeT m) where readsPrec1 = readsPrec
instance (Show1 m) => Show1 (MaybeT m) where showsPrec1 = showsPrec

-- | Transform the computation inside a @MaybeT@.
--
-- * @'runMaybeT' ('mapMaybeT' f m) = f ('runMaybeT' m)@
mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

-- | Convert a 'MaybeT' computation to 'ExceptT', with a default
-- exception value.
maybeToExceptT :: (Functor m) => e -> MaybeT m a -> ExceptT e m a
maybeToExceptT e (MaybeT m) = ExceptT $ fmap (maybe (Left e) Right) m

-- | Convert a 'ExceptT' computation to 'MaybeT', discarding the
-- value of any exception.
exceptToMaybeT :: (Functor m) => ExceptT e m a -> MaybeT m a
exceptToMaybeT (ExceptT m) = MaybeT $ fmap (either (const Nothing) Just) m

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))

instance (Foldable f) => Foldable (MaybeT f) where
    foldMap f (MaybeT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (MaybeT f) where
    traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap
 
instance (Functor m, Monad m) => Alternative (MaybeT m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    return = lift . return
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)

instance (Monad m) => MonadPlus (MaybeT m) where
    mzero = MaybeT (return Nothing)
    mplus x y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v

instance (MonadFix m) => MonadFix (MaybeT m) where
    mfix f = MaybeT (mfix (runMaybeT . f . fromMaybe bomb))
      where bomb = error "mfix (MaybeT): inner computation returned Nothing"

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m (Maybe a) (Maybe b) -> CallCC (MaybeT m) a b
liftCallCC callCC f =
    MaybeT $ callCC $ \ c -> runMaybeT (f (MaybeT . c . Just))

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (Maybe a) -> Catch e (MaybeT m) a
liftCatch f m h = MaybeT $ f (runMaybeT m) (runMaybeT . h)

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (Maybe a) -> Listen w (MaybeT m) a
liftListen listen = mapMaybeT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (Maybe a) -> Pass w (MaybeT m) a
liftPass pass = mapMaybeT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Nothing     -> (Nothing, id)
        Just (v, f) -> (Just v, f)
