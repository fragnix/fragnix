{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/GHC/Event/KQueue.hs" #-}
{-# LINE 1 "GHC/Event/KQueue.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LINE 2 "GHC/Event/KQueue.hsc" #-}
{-# LANGUAGE CApiFFI
           , GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , RecordWildCards
           , BangPatterns
  #-}

module GHC.Event.KQueue
    (
      new
    , available
    ) where

import qualified GHC.Event.Internal as E


{-# LINE 18 "GHC/Event/KQueue.hsc" #-}

{-# LINE 19 "GHC/Event/KQueue.hsc" #-}
import GHC.Base

new :: IO E.Backend
new = error "KQueue back end not implemented for this platform"

available :: Bool
available = False
{-# INLINE available #-}

{-# LINE 298 "GHC/Event/KQueue.hsc" #-}
