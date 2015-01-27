{-# LINE 1 "Foreign/Marshal/Utils/Lifted.hs" #-}
# 1 "Foreign/Marshal/Utils/Lifted.hs"
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
# 1 "Foreign/Marshal/Utils/Lifted.hs"
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
