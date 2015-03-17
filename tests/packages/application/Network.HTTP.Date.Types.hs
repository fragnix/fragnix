{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/HTTP/Date/Types.hs" #-}
module Network.HTTP.Date.Types (
    HTTPDate
  , hdYear
  , hdMonth
  , hdDay
  , hdHour
  , hdMinute
  , hdSecond
  , hdWkday
  , defaultHTTPDate
  ) where

{-|
  Data structure for HTTP Date. This value should be specified
  with 'defaultHTTPDate' and its field labels.
-}
data HTTPDate = HTTPDate {
    hdYear   :: !Int
  , hdMonth  :: !Int
  , hdDay    :: !Int
  , hdHour   :: !Int
  , hdMinute :: !Int
  , hdSecond :: !Int
  , hdWkday  :: !Int
  } deriving (Eq,Show, Ord)

{-|
  A default value for 'HTTPDate'.
-}
-- 1970/1/1 is Thu (4)
defaultHTTPDate :: HTTPDate
defaultHTTPDate = HTTPDate 1970 1 1 0 0 0 4
