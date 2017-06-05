{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/HTTP/Types/Version.hs" #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.HTTP.Types.Version
(
  HttpVersion(..)
, http09
, http10
, http11
)
where

import Data.Typeable

-- | HTTP Version.
--
-- Note that the Show instance is intended merely for debugging.
data HttpVersion
    = HttpVersion {
        httpMajor :: !Int
      , httpMinor :: !Int
      }
    deriving (Eq, Ord, Typeable)

instance Show HttpVersion where
    show (HttpVersion major minor) = "HTTP/" ++ show major ++ "." ++ show minor

-- | HTTP 0.9
http09 :: HttpVersion
http09 = HttpVersion 0 9

-- | HTTP 1.0
http10 :: HttpVersion
http10 = HttpVersion 1 0

-- | HTTP 1.1
http11 :: HttpVersion
http11 = HttpVersion 1 1
