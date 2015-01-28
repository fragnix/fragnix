{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Foreign/Marshal/Utils/Lifted.hs" #-}













































{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE Trustworthy #-}

{- |
Module      :  Foreign.Marshal.Utils.Lifted
Copyright   :  Bas van Dijk, Anders Kaseorg, Michael Steele
License     :  BSD-style

Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
Stability   :  experimental
Portability :  non-portable (extended exceptions)

This is a wrapped version of "Foreign.Marshal.Utils" with types generalized
from 'IO' to all monads in either 'MonadBase' or 'MonadBaseControl'.
-}

module Foreign.Marshal.Utils.Lifted
  ( with
  ) where

-- from base:
import qualified Foreign as F
import System.IO     ( IO )
import Prelude ( (.) )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl
                                   , liftBaseOp )

-- |Generalized version of 'F.with'.
--
-- Note, when the given function throws an exception any monadic side
-- effects in @m@ will be discarded.
with :: (MonadBaseControl IO m, F.Storable a)
     => a                -- ^ value to be poked
     -> (F.Ptr a -> m b) -- ^ computation to run
     -> m b
with = liftBaseOp . F.with
{-# INLINEABLE with #-}
