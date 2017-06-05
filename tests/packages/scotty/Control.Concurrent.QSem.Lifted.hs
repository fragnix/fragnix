{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Control/Concurrent/QSem/Lifted.hs" #-}




















































{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE Safe #-}

{- |
Module      :  Control.Concurrent.QSem.Lifted
Copyright   :  Liyang HU, Bas van Dijk
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental

This is a wrapped version of "Control.Concurrent.QSem" with types
generalised from 'IO' to all monads in 'MonadBase'.
-}

module Control.Concurrent.QSem.Lifted
    ( QSem
    , newQSem
    , waitQSem
    , signalQSem
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.QSem ( QSem )
import qualified Control.Concurrent.QSem as QSem
import Data.Int ( Int )
import System.IO ( IO )
import Prelude ( (.) )

-- from transformers-base:
import Control.Monad.Base ( MonadBase, liftBase )


--------------------------------------------------------------------------------
-- * QSems
--------------------------------------------------------------------------------

-- | Generalized version of 'QSem.newQSem'.
newQSem :: MonadBase IO m => Int -> m QSem
newQSem = liftBase . QSem.newQSem
{-# INLINABLE newQSem #-}

-- | Generalized version of 'QSem.waitQSem'.
waitQSem :: MonadBase IO m => QSem -> m ()
waitQSem = liftBase . QSem.waitQSem
{-# INLINABLE waitQSem #-}

-- | Generalized version of 'QSem.signalQSem'.
signalQSem :: MonadBase IO m => QSem -> m ()
signalQSem = liftBase . QSem.signalQSem
{-# INLINABLE signalQSem #-}
