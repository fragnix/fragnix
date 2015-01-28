{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/Wai/Middleware/Local.hs" #-}





























































































{-# LANGUAGE OverloadedStrings, CPP #-}
-- | Only allow local connections.
--
module Network.Wai.Middleware.Local
    ( local
    ) where

import Network.Wai (Middleware,remoteHost, Response)
import Network.Socket (SockAddr(..))

-- | This middleware rejects non-local connections with a specific response. 
--   It is useful when supporting web-based local applications, which would
--   typically want to reject external connections.

local :: Response -> Middleware
local resp f r k = case remoteHost r of
                   SockAddrInet _  h | h == fromIntegral home
                                    -> f r k
                   SockAddrUnix _   -> f r k
                   _                ->  k $ resp
 where
        home :: Integer
        home = 127 + (256 * 256 * 256) * 1

