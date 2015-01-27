{-# LINE 1 "src/Control/Monad/Base.hs" #-}













































{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Safe #-}


module Control.Monad.Base
  ( MonadBase(..)
  , liftBaseDefault
  ) where

import Data.Monoid
import Data.Functor.Identity
import Control.Applicative (Applicative(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Error
import Control.Monad.Trans.Cont



import qualified Control.Monad.ST.Lazy.Safe as L
import qualified Control.Monad.ST.Safe as S

import Control.Monad.STM (STM)

class (Applicative b, Applicative m, Monad b, Monad m)
      ⇒ MonadBase b m | m → b where
  -- | Lift a computation from the base monad
  liftBase ∷ b α → m α


instance MonadBase (IO) (IO) where liftBase = id
instance MonadBase (Maybe) (Maybe) where liftBase = id
instance MonadBase (Either e) (Either e) where liftBase = id
instance MonadBase ([]) ([]) where liftBase = id
instance MonadBase ((→) r) ((→) r) where liftBase = id
instance MonadBase (Identity) (Identity) where liftBase = id

instance MonadBase (STM) (STM) where liftBase = id


instance MonadBase (L.ST s) (L.ST s) where liftBase = id
instance MonadBase (S.ST s) (S.ST s) where liftBase = id


-- | Can be used as a default implementation for 'liftBase'.
--
-- Note that: @liftBaseDefault = 'lift' . 'liftBase'@
liftBaseDefault ∷ (MonadTrans t, MonadBase b m) ⇒ b α → t m α
liftBaseDefault = lift . liftBase


instance (MonadBase b m) ⇒ MonadBase b (IdentityT m) where liftBase = liftBaseDefault
instance (MonadBase b m) ⇒ MonadBase b (MaybeT m) where liftBase = liftBaseDefault
instance (MonadBase b m) ⇒ MonadBase b (ListT m) where liftBase = liftBaseDefault
instance (MonadBase b m) ⇒ MonadBase b (ReaderT r m) where liftBase = liftBaseDefault
instance (MonadBase b m) ⇒ MonadBase b (L.StateT s m) where liftBase = liftBaseDefault
instance (MonadBase b m) ⇒ MonadBase b (S.StateT s m) where liftBase = liftBaseDefault
instance (MonadBase b m) ⇒ MonadBase b (ContT r m) where liftBase = liftBaseDefault


instance (Monoid w, MonadBase b m) ⇒ MonadBase b ( L.WriterT w m) where liftBase = liftBaseDefault
instance (Monoid w, MonadBase b m) ⇒ MonadBase b ( S.WriterT w m) where liftBase = liftBaseDefault
instance (Monoid w, MonadBase b m) ⇒ MonadBase b ( L.RWST r w s m) where liftBase = liftBaseDefault
instance (Monoid w, MonadBase b m) ⇒ MonadBase b ( S.RWST r w s m) where liftBase = liftBaseDefault
instance (Error e, MonadBase b m) ⇒ MonadBase b (  ErrorT e m) where liftBase = liftBaseDefault
