{-# LINE 1 "Control/Concurrent/QSem/Lifted.hs" #-}
# 1 "Control/Concurrent/QSem/Lifted.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1





























































































# 8 "<command-line>" 2
# 1 "Control/Concurrent/QSem/Lifted.hs"
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE Trustworthy #-}


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


# 1 "include/inlinable.h" 1
# 44 "Control/Concurrent/QSem/Lifted.hs" 2

--------------------------------------------------------------------------------
-- * QSems
--------------------------------------------------------------------------------

-- | Generalized version of 'QSem.newQSem'.
newQSem :: MonadBase IO m => Int -> m QSem
newQSem = liftBase . QSem.newQSem
{-# INLINE newQSem #-}

-- | Generalized version of 'QSem.waitQSem'.
waitQSem :: MonadBase IO m => QSem -> m ()
waitQSem = liftBase . QSem.waitQSem
{-# INLINE waitQSem #-}

-- | Generalized version of 'QSem.signalQSem'.
signalQSem :: MonadBase IO m => QSem -> m ()
signalQSem = liftBase . QSem.signalQSem
{-# INLINE signalQSem #-}
