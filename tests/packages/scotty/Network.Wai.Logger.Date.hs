{-# LINE 1 "Network/Wai/Logger/Date.hs" #-}
# 1 "Network/Wai/Logger/Date.hs"
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
# 1 "Network/Wai/Logger/Date.hs"
{-# LANGUAGE CPP, OverloadedStrings #-}

-- |
-- Formatting time is slow.
-- This package provides mechanisms to cache formatted date.
module Network.Wai.Logger.Date (
  -- * Types
    DateCacheGetter
  , DateCacheUpdater
  , ZonedDate
  -- * Cache configuration
  , DateCacheConf(..)
  , zonedDateCacheConf
  -- * Date cacher
  , clockDateCacher
  ) where

import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, updateAction)
import Data.ByteString (ByteString)





import Data.UnixTime (formatUnixTime, fromEpochTime)
import System.Posix (EpochTime, epochTime)


----------------------------------------------------------------

-- | Getting cached 'ZonedDate'.
type DateCacheGetter = IO ZonedDate
-- | Updateing cached 'ZonedDate'. This should be called every second.
--   See the source code of 'withStdoutLogger'.
type DateCacheUpdater = IO ()

----------------------------------------------------------------

-- | A type for zoned date.
type ZonedDate = ByteString

----------------------------------------------------------------

data DateCacheConf t = DateCacheConf {
    -- | A function to get a time. E.g 'epochTime' and 'getCurrentTime'.
    getTime :: IO t
    -- | A function to format a time.
  , formatDate :: t -> IO ByteString
  }

# 61 "Network/Wai/Logger/Date.hs"
-- | Zoned date cacher using UnixTime.
zonedDateCacheConf :: DateCacheConf EpochTime
zonedDateCacheConf = DateCacheConf {
    getTime = epochTime
  , formatDate = formatUnixTime "%d/%b/%Y:%T %z" . fromEpochTime
  }


----------------------------------------------------------------

-- |
-- Returning 'DateCacheGetter' and 'DateCacheUpdater'.
--
-- Note: Since version 2.1.2, this function uses the auto-update package
-- internally, and therefore the @DateCacheUpdater@ value returned need
-- not be called. To wit, the return value is in fact an empty action.

clockDateCacher :: IO (DateCacheGetter, DateCacheUpdater)
clockDateCacher = do
    getter <- mkAutoUpdate defaultUpdateSettings
        { updateAction = getTime zonedDateCacheConf
                     >>= formatDate zonedDateCacheConf
        }
    return (getter, return ())
