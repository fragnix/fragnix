{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Control/Monad/Signatures.hs" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Signatures
-- Copyright   :  (c) Ross Paterson 2012
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Signatures for monad operations that require specialized lifting.
-----------------------------------------------------------------------------

module Control.Monad.Signatures (
    CallCC, Catch, Listen, Pass
  ) where

-- | Signature of the @callCC@ operation,
-- introduced in "Control.Monad.Trans.Cont".
type CallCC m a b = ((a -> m b) -> m a) -> m a

-- | Signature of the @catchE@ operation,
-- introduced in "Control.Monad.Trans.Except".
type Catch e m a = m a -> (e -> m a) -> m a

-- | Signature of the @listen@ operation,
-- introduced in "Control.Monad.Trans.Writer".
type Listen w m a = m a -> m (a, w)

-- | Signature of the @pass@ operation,
-- introduced in "Control.Monad.Trans.Writer".
type Pass w m a =  m (a, w -> w) -> m a
