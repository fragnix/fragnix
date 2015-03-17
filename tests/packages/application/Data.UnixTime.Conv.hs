{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/UnixTime/Conv.hs" #-}
{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

module Data.UnixTime.Conv (
    formatUnixTime, formatUnixTimeGMT
  , parseUnixTime, parseUnixTimeGMT
  , webDateFormat, mailDateFormat
  , fromEpochTime, toEpochTime
  , fromClockTime, toClockTime
  ) where

import Control.Applicative
import Data.ByteString
import Data.ByteString.Unsafe
import Data.UnixTime.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (EpochTime)
import System.Time (ClockTime(..))

foreign import ccall unsafe "c_parse_unix_time"
        c_parse_unix_time :: CString -> CString -> IO CTime

foreign import ccall unsafe "c_parse_unix_time_gmt"
        c_parse_unix_time_gmt :: CString -> CString -> IO CTime

foreign import ccall unsafe "c_format_unix_time"
        c_format_unix_time :: CString -> CTime -> CString -> CInt -> IO CSize

foreign import ccall unsafe "c_format_unix_time_gmt"
        c_format_unix_time_gmt :: CString -> CTime -> CString -> CInt -> IO CSize

----------------------------------------------------------------

-- |
-- Parsing 'ByteString' to 'UnixTime' interpreting as localtime.
-- This is a wrapper for strptime_l().
-- Many implementations of strptime_l() do not support %Z and
-- some implementations of strptime_l() do not support %z, either.

parseUnixTime :: Format -> ByteString -> UnixTime
parseUnixTime fmt str = unsafePerformIO $
    useAsCString fmt $ \cfmt ->
        useAsCString str $ \cstr -> do
            sec <- c_parse_unix_time cfmt cstr
            return $ UnixTime sec 0
-- |
-- Parsing 'ByteString' to 'UnixTime' interpreting as GMT.
-- This is a wrapper for strptime_l().
--
-- >>> parseUnixTimeGMT webDateFormat "Thu, 01 Jan 1970 00:00:00 GMT"
-- UnixTime {utSeconds = 0, utMicroSeconds = 0}

parseUnixTimeGMT :: Format -> ByteString -> UnixTime
parseUnixTimeGMT fmt str = unsafePerformIO $
    useAsCString fmt $ \cfmt ->
        useAsCString str $ \cstr -> do
            sec <- c_parse_unix_time_gmt cfmt cstr
            return $ UnixTime sec 0

----------------------------------------------------------------

-- |
-- Formatting 'UnixTime' to 'ByteString' in local time.
-- This is a wrapper for strftime_l().

formatUnixTime :: Format -> UnixTime -> IO ByteString
formatUnixTime fmt t =
    formatUnixTimeHelper c_format_unix_time fmt t
{-# INLINE formatUnixTime #-}

-- |
-- Formatting 'UnixTime' to 'ByteString' in GMT.
-- This is a wrapper for strftime_l().
--
-- >>> formatUnixTimeGMT webDateFormat $ UnixTime 0 0
-- "Thu, 01 Jan 1970 00:00:00 GMT"

formatUnixTimeGMT :: Format -> UnixTime -> ByteString
formatUnixTimeGMT fmt t =
    unsafePerformIO $ formatUnixTimeHelper c_format_unix_time_gmt fmt t
{-# INLINE formatUnixTimeGMT #-}

-- |
-- Helper handling memory allocation for formatUnixTime and formatUnixTimeGMT.

formatUnixTimeHelper
    :: (CString -> CTime -> CString -> CInt -> IO CSize)
    -> Format
    -> UnixTime
    -> IO ByteString
formatUnixTimeHelper formatFun fmt (UnixTime sec _) =
    useAsCString fmt $ \cfmt -> do
        let siz = 80
        ptr  <- mallocBytes siz
        len  <- fromIntegral <$> formatFun cfmt sec ptr (fromIntegral siz)
        ptr' <- reallocBytes ptr (len + 1)
        unsafePackMallocCString ptr' -- FIXME: Use unsafePackMallocCStringLen from bytestring-0.10.2.0

----------------------------------------------------------------

-- |
-- Format for web (RFC 2616).
-- The value is \"%a, %d %b %Y %H:%M:%S GMT\".
-- This should be used with 'formatUnixTimeGMT' and 'parseUnixTimeGMT'.

webDateFormat :: Format
webDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

-- |
-- Format for e-mail (RFC 5322).
-- The value is \"%a, %d %b %Y %H:%M:%S %z\".
-- This should be used with 'formatUnixTime' and 'parseUnixTime'.

mailDateFormat :: Format
mailDateFormat = "%a, %d %b %Y %H:%M:%S %z"

----------------------------------------------------------------

-- |
-- From 'EpochTime' to 'UnixTime' setting 'utMicroSeconds' to 0.

fromEpochTime :: EpochTime -> UnixTime
fromEpochTime sec = UnixTime sec 0

-- |
-- From 'UnixTime' to 'EpochTime' ignoring 'utMicroSeconds'.

toEpochTime :: UnixTime -> EpochTime
toEpochTime (UnixTime sec _) = sec

-- |
-- From 'ClockTime' to 'UnixTime'.

fromClockTime :: ClockTime -> UnixTime
fromClockTime (TOD sec psec) = UnixTime sec' usec'
  where
    sec' = fromIntegral sec
    usec' = fromIntegral $ psec `div` 1000000

-- |
-- From 'UnixTime' to 'ClockTime'.

toClockTime :: UnixTime -> ClockTime
toClockTime (UnixTime sec usec) = TOD sec' psec'
  where
    sec' = truncate (toRational sec)
    psec' = fromIntegral $ usec * 1000000
