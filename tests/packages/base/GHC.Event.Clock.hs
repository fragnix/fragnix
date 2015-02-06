{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/GHC/Event/Clock.hs" #-}
{-# LINE 1 "GHC/Event/Clock.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LINE 2 "GHC/Event/Clock.hsc" #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Event.Clock (getMonotonicTime) where

import GHC.Base
import GHC.Real
import Data.Word

-- | Return monotonic time in seconds, since some unspecified starting point
getMonotonicTime :: IO Double
getMonotonicTime = do w <- getMonotonicNSec
                      return (fromIntegral w / 1000000000)

foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicNSec :: IO Word64

