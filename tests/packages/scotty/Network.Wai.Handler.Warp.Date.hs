{-# LINE 1 "Network/Wai/Handler/Warp/Date.hs" #-}
# 1 "Network/Wai/Handler/Warp/Date.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1













































































































































































































# 9 "<command-line>" 2
# 1 "Network/Wai/Handler/Warp/Date.hs"
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Date (
    withDateCache
  , getDate
  , DateCache
  , GMTDate
  ) where

import Control.Applicative
import Control.AutoUpdate (defaultUpdateSettings, updateAction, mkAutoUpdate)
import Data.ByteString.Char8





import Network.HTTP.Date
import System.Posix (epochTime)


-- | The type of the Date header value.
type GMTDate = ByteString

-- | The type of the cache of the Date header value.
type DateCache = IO GMTDate

-- | Creating 'DateCache' and executing the action.
withDateCache :: (DateCache -> IO a) -> IO a
withDateCache action = initialize >>= action

initialize :: IO DateCache
initialize = mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentGMTDate }

-- | Getting 'GMTDate' based on 'DateCache'.
getDate :: DateCache -> IO GMTDate
getDate = id

getCurrentGMTDate :: IO GMTDate





getCurrentGMTDate = formatHTTPDate . epochTimeToHTTPDate <$> epochTime
