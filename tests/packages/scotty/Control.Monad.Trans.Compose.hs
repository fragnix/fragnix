{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Control/Monad/Trans/Compose.hs" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Composition of monad transformers. A higher-order version of
    "Data.Functor.Compose".
-}

module Control.Monad.Trans.Compose (
    -- * ComposeT
    ComposeT(ComposeT, getComposeT),
    mapComposeT
   ) where

import Control.Applicative (
    Applicative(pure, (<*>), (*>), (<*)), Alternative(empty, (<|>)) )
import Control.Monad (MonadPlus(mzero, mplus), liftM)
import Control.Monad.Cont.Class (MonadCont(callCC))
import Control.Monad.Error.Class (MonadError(throwError, catchError))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader(ask, local, reader))
import Control.Monad.State.Class (MonadState(get, put, state))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Writer.Class (MonadWriter(writer, tell, listen, pass))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (Foldable(fold, foldMap, foldr, foldl, foldr1, foldl1))
import Data.Traversable (Traversable(traverse, sequenceA, mapM, sequence))
import Prelude hiding (foldr, foldl, foldr1, foldl1, mapM, sequence)

infixr 9 `ComposeT`

-- | Composition of monad transformers.
newtype ComposeT (f :: (* -> *) -> * -> *) (g :: (* -> *) -> * -> *) m a
    = ComposeT { getComposeT :: f (g m) a }
  deriving (Eq, Ord, Read, Show)

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

instance MonadCont (f (g m)) => MonadCont (ComposeT f g m) where
    callCC f = ComposeT $ callCC $ \c -> getComposeT (f (ComposeT . c))

instance MonadError e (f (g m)) => MonadError e (ComposeT f g m) where
    throwError     = ComposeT . throwError
    catchError m h = ComposeT $ catchError (getComposeT m) (getComposeT . h)

instance MonadRWS r w s (f (g m)) => MonadRWS r w s (ComposeT f g m)

instance MonadReader r (f (g m)) => MonadReader r (ComposeT f g m) where
    ask    = ComposeT ask
    local  = mapComposeT . local
    reader = ComposeT . reader

instance MonadState s (f (g m)) => MonadState s (ComposeT f g m) where
    get   = ComposeT get
    put   = ComposeT . put
    state = ComposeT . state

instance MonadWriter w (f (g m)) => MonadWriter w (ComposeT f g m) where
    writer = ComposeT . writer
    tell   = ComposeT . tell
    listen = mapComposeT listen
    pass   = mapComposeT pass

-- | Transform the computation inside a 'ComposeT'.
mapComposeT :: (f (g m) a -> p (q n) b) -> ComposeT f g m a -> ComposeT p q n b
mapComposeT f = ComposeT . f . getComposeT
