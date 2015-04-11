{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Control/Monad/Trans/Except.hs" #-}











































{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Except
-- Copyright   :  (C) 2013 Ross Paterson
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- This monad transformer extends a monad with the ability throw exceptions.
--
-- A sequence of actions terminates normally, producing a value,
-- only if none of the actions in the sequence throws an exception.
-- If one throws an exception, the rest of the sequence is skipped and
-- the composite action exits with that exception.
--
-- If the value of the exception is not required, the variant in
-- "Control.Monad.Trans.Maybe" may be used instead.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Except (
    -- * The Except monad
    Except,
    except,
    runExcept,
    mapExcept,
    withExcept,
    -- * The ExceptT monad transformer
    ExceptT(ExceptT),
    runExceptT,
    mapExceptT,
    withExceptT,
    -- * Exception operations
    throwE,
    catchE,
    -- * Lifting other operations
    liftCallCC,
    liftListen,
    liftPass,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Functor.Classes
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Foldable (Foldable(foldMap))
import Data.Monoid
import Data.Traversable (Traversable(traverse))

-- | The parameterizable exception monad.
--
-- Computations are either exceptions or normal values.
--
-- The 'return' function returns a normal value, while @>>=@ exits on
-- the first exception.  For a variant that continues after an error
-- and collects all the errors, see 'Control.Applicative.Lift.Errors'.
type Except e = ExceptT e Identity

-- | Constructor for computations in the exception monad.
-- (The inverse of 'runExcept').
except :: Either e a -> Except e a
except m = ExceptT (Identity m)

-- | Extractor for computations in the exception monad.
-- (The inverse of 'except').
runExcept :: Except e a -> Either e a
runExcept (ExceptT m) = runIdentity m

-- | Map the unwrapped computation using the given function.
--
-- * @'runExcept' ('mapExcept' f m) = f ('runExcept' m)@
mapExcept :: (Either e a -> Either e' b)
        -> Except e a
        -> Except e' b
mapExcept f = mapExceptT (Identity . f . runIdentity)

-- | Transform any exceptions thrown by the computation using the given
-- function (a specialization of 'withExceptT').
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = withExceptT

-- | A monad transformer that adds exceptions to other monads.
--
-- @ExceptT@ constructs a monad parameterized over two things:
--
-- * e - The exception type.
--
-- * m - The inner monad.
--
-- The 'return' function yields a computation that produces the given
-- value, while @>>=@ sequences two subcomputations, exiting on the
-- first exception.
newtype ExceptT e m a = ExceptT (m (Either e a))

instance (Eq e, Eq1 m, Eq a) => Eq (ExceptT e m a) where
    ExceptT x == ExceptT y = eq1 x y

instance (Ord e, Ord1 m, Ord a) => Ord (ExceptT e m a) where
    compare (ExceptT x) (ExceptT y) = compare1 x y

instance (Read e, Read1 m, Read a) => Read (ExceptT e m a) where
    readsPrec = readsData $ readsUnary1 "ExceptT" ExceptT

instance (Show e, Show1 m, Show a) => Show (ExceptT e m a) where
    showsPrec d (ExceptT m) = showsUnary1 "ExceptT" d m

instance (Eq e, Eq1 m) => Eq1 (ExceptT e m) where eq1 = (==)
instance (Ord e, Ord1 m) => Ord1 (ExceptT e m) where compare1 = compare
instance (Read e, Read1 m) => Read1 (ExceptT e m) where readsPrec1 = readsPrec
instance (Show e, Show1 m) => Show1 (ExceptT e m) where showsPrec1 = showsPrec

-- | The inverse of 'ExceptT'.
runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (ExceptT m) = m

-- | Map the unwrapped computation using the given function.
--
-- * @'runExceptT' ('mapExceptT' f m) = f ('runExceptT' m)@
mapExceptT :: (m (Either e a) -> n (Either e' b))
        -> ExceptT e m a
        -> ExceptT e' n b
mapExceptT f m = ExceptT $ f (runExceptT m)

-- | Transform any exceptions thrown by the computation using the
-- given function.
withExceptT :: (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT $ fmap $ either (Left . f) Right

instance (Functor m) => Functor (ExceptT e m) where
    fmap f = ExceptT . fmap (fmap f) . runExceptT

instance (Foldable f) => Foldable (ExceptT e f) where
    foldMap f (ExceptT a) = foldMap (either (const mempty) f) a

instance (Traversable f) => Traversable (ExceptT e f) where
    traverse f (ExceptT a) =
        ExceptT <$> traverse (either (pure . Left) (fmap Right . f)) a

instance (Functor m, Monad m) => Applicative (ExceptT e m) where
    pure a = ExceptT $ return (Right a)
    ExceptT f <*> ExceptT v = ExceptT $ do
        mf <- f
        case mf of
            Left e -> return (Left e)
            Right k -> do
                mv <- v
                case mv of
                    Left e -> return (Left e)
                    Right x -> return (Right (k x))

instance (Functor m, Monad m, Monoid e) => Alternative (ExceptT e m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (ExceptT e m) where
    return a = ExceptT $ return (Right a)
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left e -> return (Left e)
            Right x -> runExceptT (k x)
    fail = ExceptT . fail

instance (Monad m, Monoid e) => MonadPlus (ExceptT e m) where
    mzero = ExceptT $ return (Left mempty)
    ExceptT m `mplus` ExceptT n = ExceptT $ do
        a <- m
        case a of
            Left e -> liftM (either (Left . mappend e) Right) n
            Right x -> return (Right x)

instance (MonadFix m) => MonadFix (ExceptT e m) where
    mfix f = ExceptT (mfix (runExceptT . f . either (const bomb) id))
      where bomb = error "mfix (ExceptT): inner computation returned Left value"

instance MonadTrans (ExceptT e) where
    lift = ExceptT . liftM Right

instance (MonadIO m) => MonadIO (ExceptT e m) where
    liftIO = lift . liftIO

-- | Signal an exception value @e@.
--
-- * @'runExceptT' ('throwE' e) = 'return' ('Left' e)@
--
-- * @'throwE' e >>= m = 'throwE' e@
throwE :: (Monad m) => e -> ExceptT e m a
throwE = ExceptT . return . Left

-- | Handle an exception.
--
-- * @'catchE' h ('lift' m) = 'lift' m@
--
-- * @'catchE' h ('throwE' e) = h e@
catchE :: (Monad m) =>
    ExceptT e m a               -- ^ the inner computation
    -> (e -> ExceptT e' m a)    -- ^ a handler for exceptions in the inner
                                -- computation
    -> ExceptT e' m a
m `catchE` h = ExceptT $ do
    a <- runExceptT m
    case a of
        Left  l -> runExceptT (h l)
        Right r -> return (Right r)

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m (Either e a) (Either e b) -> CallCC (ExceptT e m) a b
liftCallCC callCC f = ExceptT $
    callCC $ \ c ->
    runExceptT (f (\ a -> ExceptT $ c (Right a)))

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (Either e a) -> Listen w (ExceptT e m) a
liftListen listen = mapExceptT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (Either e a) -> Pass w (ExceptT e m) a
liftPass pass = mapExceptT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Left l -> (Left l, id)
        Right (r, f) -> (Right r, f)
