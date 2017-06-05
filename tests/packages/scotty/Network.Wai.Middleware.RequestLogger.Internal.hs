{-# LANGUAGE Haskell2010, OverloadedStrings #-}
{-# LINE 1 "Network/Wai/Middleware/RequestLogger/Internal.hs" #-}











































































































































{-# LANGUAGE CPP #-}
-- | A module for containing some CPPed code, due to:
--
-- https://github.com/yesodweb/wai/issues/192
module Network.Wai.Middleware.RequestLogger.Internal
    ( module Network.Wai.Middleware.RequestLogger.Internal
    ) where

import Data.ByteString (ByteString)
import Network.Wai.Logger (clockDateCacher)
import System.Log.FastLogger (LogStr, fromLogStr)

logToByteString :: LogStr -> ByteString
logToByteString = fromLogStr

getDateGetter :: IO () -- ^ flusher
              -> IO (IO ByteString)
getDateGetter _ = do
    (getter, _) <- clockDateCacher
    return getter
