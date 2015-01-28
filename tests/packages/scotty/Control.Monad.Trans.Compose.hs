{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Control/Monad/Trans/Compose.hs" #-}
{-# LANGUAGE FlexibleContexts, KindSignatures #-}

{-| Composition of monad transformers. A higher-order version of
    "Data.Functor.Compose".
-}

module Control.Monad.Trans.Compose (
    -- * ComposeT
    ComposeT(ComposeT, getComposeT),
   ) where

import Control.Applicative (
    Applicative(pure, (<*>), (*>), (<*)), Alternative(empty, (<|>)) )
import Control.Monad (MonadPlus(mzero, mplus), liftM)
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (Foldable(fold, foldMap, foldr, foldl, foldr1, foldl1))
import Data.Traversable (Traversable(traverse, sequenceA, mapM, sequence))
import Prelude hiding (foldr, foldl, foldr1, foldl1, mapM, sequence)

-- | Composition of monad transformers.
newtype ComposeT (f :: (* -> *) -> * -> *) (g :: (* -> *) -> * -> *) m a
    = ComposeT { getComposeT :: f (g m) a }

instance (MFunctor f, MonadTrans f, MonadTrans g) => MonadTrans (ComposeT f g)
  where
    lift = ComposeT . hoist lift . lift

instance Functor (f (g m)) => Functor (ComposeT f g m) where
    fmap f (ComposeT m) = ComposeT (fmap f m)

instance Applicative (f (g m)) => Applicative (ComposeT f g m) where
    pure a = ComposeT (pure a)
    ComposeT f <*> ComposeT a = ComposeT (f <*> a)
    ComposeT a *> ComposeT b = ComposeT (a *> b)
    ComposeT a <* ComposeT b = ComposeT (a <* b)

instance Alternative (f (g m)) => Alternative (ComposeT f g m) where
    empty = ComposeT empty
    ComposeT a <|> ComposeT b = ComposeT (a <|> b)

instance Monad (f (g m)) => Monad (ComposeT f g m) where
    return a = ComposeT (return a)
    m >>= f  = ComposeT (getComposeT m >>= \x -> getComposeT (f x))
    fail e   = ComposeT (fail e)

instance MonadPlus (f (g m)) => MonadPlus (ComposeT f g m) where
    mzero = ComposeT mzero
    ComposeT a `mplus` ComposeT b = ComposeT (a `mplus` b)

instance MonadIO (f (g m)) => MonadIO (ComposeT f g m) where
    liftIO m = ComposeT (liftIO m)

instance Foldable (f (g m)) => Foldable (ComposeT f g m) where
    fold        (ComposeT m) = fold m
    foldMap f   (ComposeT m) = foldMap f   m
    foldr   f a (ComposeT m) = foldr   f a m
    foldl   f a (ComposeT m) = foldl   f a m
    foldr1 f    (ComposeT m) = foldr1  f   m
    foldl1 f    (ComposeT m) = foldl1  f   m

instance Traversable (f (g m)) => Traversable (ComposeT f g m) where
    traverse f (ComposeT m) = fmap  ComposeT (traverse f m)
    sequenceA  (ComposeT m) = fmap  ComposeT (sequenceA  m)
    mapM     f (ComposeT m) = liftM ComposeT (mapM     f m)
    sequence   (ComposeT m) = liftM ComposeT (sequence   m)
