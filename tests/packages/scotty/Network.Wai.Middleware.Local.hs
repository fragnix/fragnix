{-# LINE 1 "Network/Wai/Middleware/Local.hs" #-}
# 1 "Network/Wai/Middleware/Local.hs"
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
# 1 "Network/Wai/Middleware/Local.hs"
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

