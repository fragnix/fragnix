{-# LINE 1 "System/Timeout/Lifted.hs" #-}













































{-# LANGUAGE CPP, NoImplicitPrelude, FlexibleContexts #-}

{-# LANGUAGE Trustworthy #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.Timeout.Lifted
-- Copyright   :  (c) The University of Glasgow 2007
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Attach a timeout event to monadic computations
-- which are instances of 'MonadBaseControl'.
--
-------------------------------------------------------------------------------

module System.Timeout.Lifted ( timeout ) where

-- from base:
import Prelude                       ( (.) )
import           Data.Int            ( Int )
import           Data.Maybe          ( Maybe(Nothing, Just), maybe )
import           Control.Monad       ( (>>=), return, liftM )
import           System.IO           ( IO )
import qualified System.Timeout as T ( timeout )

-- from monad-control:
import Control.Monad.Trans.Control ( MonadBaseControl, restoreM, liftBaseWith )


-- | Generalized version of 'T.timeout'.
--
-- Note that when the given computation times out any side effects of @m@ are
-- discarded. When the computation completes within the given time the
-- side-effects are restored on return.
timeout :: MonadBaseControl IO m => Int -> m a -> m (Maybe a)
timeout t m = liftBaseWith (\runInIO -> T.timeout t (runInIO m)) >>=
                maybe (return Nothing) (liftM Just . restoreM)
{-# INLINE timeout #-}
