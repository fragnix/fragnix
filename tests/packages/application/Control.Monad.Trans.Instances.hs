{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Control/Monad/Trans/Instances.hs" #-}




















































{-# LANGUAGE CPP #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Instances
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Backports orphan instances which are not provided by other modules in
-- @transformers-compat@.
----------------------------------------------------------------------------
module Control.Monad.Trans.Instances () where



import           Control.Applicative.Backwards (Backwards(..))
import           Control.Applicative.Lift (Lift(..))
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Cont (ContT(..))
import           Control.Monad.Trans.Error (ErrorT(..))
import           Control.Monad.Trans.Except ()
import           Control.Monad.Trans.Identity (IdentityT(..))
import           Control.Monad.Trans.List (ListT(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT(..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT(..))
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..))
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))
import           Data.Functor.Classes
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Constant (Constant(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Functor.Product (Product(..))
import           Data.Functor.Reverse (Reverse(..))
import           Data.Functor.Sum ()

import           Control.Applicative
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Fix (MonadFix(..))
import           Data.Foldable (Foldable(..))
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..))
import           Data.Traversable (Traversable(..))

import           Control.Monad.Zip (MonadZip(..))

import           Data.Bifunctor (Bifunctor(..))

import           Data.Data (Data)
import           Data.Typeable

import           GHC.Generics




