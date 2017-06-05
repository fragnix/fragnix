{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/Wai/Handler/Warp/Date.hs" #-}






















































































































{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Date (
    withDateCache
  , GMTDate
  ) where

import Control.AutoUpdate (defaultUpdateSettings, updateAction, mkAutoUpdate)
import Data.ByteString.Char8
import Network.HTTP.Date

import System.Posix (epochTime)

-- | The type of the Date header value.
type GMTDate = ByteString

-- | Creating 'DateCache' and executing the action.
withDateCache :: (IO GMTDate -> IO a) -> IO a
withDateCache action = initialize >>= action

initialize :: IO (IO GMTDate)
initialize = mkAutoUpdate defaultUpdateSettings {
                            updateAction = formatHTTPDate <$> getCurrentHTTPDate
                          }

getCurrentHTTPDate :: IO HTTPDate
getCurrentHTTPDate = epochTimeToHTTPDate <$> epochTime
