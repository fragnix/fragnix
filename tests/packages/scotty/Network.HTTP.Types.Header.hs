{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/HTTP/Types/Header.hs" #-}





























































{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, CPP #-}
module Network.HTTP.Types.Header
(
  -- ** Types
  Header
, HeaderName
, RequestHeaders
, ResponseHeaders
  -- ** Common headers
, hAccept
, hAcceptCharset
, hAcceptEncoding
, hAcceptLanguage
, hAcceptRanges
, hAge
, hAllow
, hAuthorization
, hCacheControl
, hConnection
, hContentEncoding
, hContentLanguage
, hContentLength
, hContentLocation
, hContentMD5
, hContentRange
, hContentType
, hCookie
, hDate
, hETag
, hExpect
, hExpires
, hFrom
, hHost
, hIfMatch
, hIfModifiedSince
, hIfNoneMatch
, hIfRange
, hIfUnmodifiedSince
, hLastModified
, hLocation
, hMaxForwards
, hPragma
, hProxyAuthenticate
, hProxyAuthorization
, hRange
, hReferer
, hRetryAfter
, hServer
, hTE
, hTrailer
, hTransferEncoding
, hUpgrade
, hUserAgent
, hVary
, hVia
, hWWWAuthenticate
, hWarning
  -- ** Byte ranges
, ByteRange(..)
, renderByteRangeBuilder
, renderByteRange
, ByteRanges
, renderByteRangesBuilder
, renderByteRanges
, parseByteRanges
)
where

import           Data.List
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.CaseInsensitive           as CI
import           Data.ByteString.Char8          () {-IsString-}
import           Data.Typeable                  (Typeable)
import           Data.Data                      (Data)

-- | Header
type Header = (HeaderName, B.ByteString)

-- | Header name
type HeaderName = CI.CI B.ByteString

-- | Request Headers
type RequestHeaders = [Header]

-- | Response Headers
type ResponseHeaders = [Header]

-- | HTTP Header names
-- According to http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
hAccept, hAcceptCharset, hAcceptEncoding, hAcceptLanguage, hAcceptRanges, hAge, hAllow, hAuthorization, hCacheControl, hConnection, hContentEncoding, hContentLanguage, hContentLength, hContentLocation, hContentMD5, hContentRange, hContentType, hCookie, hDate, hETag, hExpect, hExpires, hFrom, hHost, hIfMatch, hIfModifiedSince, hIfNoneMatch, hIfRange, hIfUnmodifiedSince, hLastModified, hLocation, hMaxForwards, hPragma, hProxyAuthenticate, hProxyAuthorization, hRange, hReferer, hRetryAfter, hServer, hTE, hTrailer, hTransferEncoding, hUpgrade, hUserAgent, hVary, hVia, hWWWAuthenticate, hWarning :: HeaderName
hAccept             = "Accept"
hAcceptCharset      = "Accept-Charset"
hAcceptEncoding     = "Accept-Encoding"
hAcceptLanguage     = "Accept-Language"
hAcceptRanges       = "Accept-Ranges"
hAge                = "Age"
hAllow              = "Allow"
hAuthorization      = "Authorization"
hCacheControl       = "Cache-Control"
hConnection         = "Connection"
hContentEncoding    = "Content-Encoding"
hContentLanguage    = "Content-Language"
hContentLength      = "Content-Length"
hContentLocation    = "Content-Location"
hContentMD5         = "Content-MD5"
hContentRange       = "Content-Range"
hContentType        = "Content-Type"
hCookie             = "Cookie"
hDate               = "Date"
hETag               = "ETag"
hExpect             = "Expect"
hExpires            = "Expires"
hFrom               = "From"
hHost               = "Host"
hIfMatch            = "If-Match"
hIfModifiedSince    = "If-Modified-Since"
hIfNoneMatch        = "If-None-Match"
hIfRange            = "If-Range"
hIfUnmodifiedSince  = "If-Unmodified-Since"
hLastModified       = "Last-Modified"
hLocation           = "Location"
hMaxForwards        = "Max-Forwards"
hPragma             = "Pragma"
hProxyAuthenticate  = "Proxy-Authenticate"
hProxyAuthorization = "Proxy-Authorization"
hRange              = "Range"
hReferer            = "Referer"
hRetryAfter         = "Retry-After"
hServer             = "Server"
hTE                 = "TE"
hTrailer            = "Trailer"
hTransferEncoding   = "Transfer-Encoding"
hUpgrade            = "Upgrade"
hUserAgent          = "User-Agent"
hVary               = "Vary"
hVia                = "Via"
hWWWAuthenticate    = "WWW-Authenticate"
hWarning            = "Warning"

-- | RFC 2616 Byte range (individual).
--
-- Negative indices are not allowed!
data ByteRange
  = ByteRangeFrom !Integer
  | ByteRangeFromTo !Integer !Integer
  | ByteRangeSuffix !Integer
  deriving (Show, Eq, Ord, Typeable, Data)

renderByteRangeBuilder :: ByteRange -> Blaze.Builder
renderByteRangeBuilder (ByteRangeFrom from) = Blaze.fromShow from `mappend` Blaze.fromChar '-'
renderByteRangeBuilder (ByteRangeFromTo from to) = Blaze.fromShow from `mappend` Blaze.fromChar '-' `mappend` Blaze.fromShow to
renderByteRangeBuilder (ByteRangeSuffix suffix) = Blaze.fromChar '-' `mappend` Blaze.fromShow suffix

renderByteRange :: ByteRange -> B.ByteString
renderByteRange = Blaze.toByteString . renderByteRangeBuilder

-- | RFC 2616 Byte ranges (set).
type ByteRanges = [ByteRange]

renderByteRangesBuilder :: ByteRanges -> Blaze.Builder
renderByteRangesBuilder xs = Blaze.copyByteString "bytes=" `mappend` 
                             mconcat (intersperse (Blaze.fromChar ',') (map renderByteRangeBuilder xs))

renderByteRanges :: ByteRanges -> B.ByteString
renderByteRanges = Blaze.toByteString . renderByteRangesBuilder

-- | Parse the value of a Range header into a 'ByteRanges'.
--
-- >>> parseByteRanges "error"
-- Nothing
-- >>> parseByteRanges "bytes=0-499"
-- Just [ByteRangeFromTo 0 499]
-- >>> parseByteRanges "bytes=500-999"
-- Just [ByteRangeFromTo 500 999]
-- >>> parseByteRanges "bytes=-500"
-- Just [ByteRangeSuffix 500]
-- >>> parseByteRanges "bytes=9500-"
-- Just [ByteRangeFrom 9500]
-- >>> parseByteRanges "bytes=0-0,-1"
-- Just [ByteRangeFromTo 0 0,ByteRangeSuffix 1]
-- >>> parseByteRanges "bytes=500-600,601-999"
-- Just [ByteRangeFromTo 500 600,ByteRangeFromTo 601 999]
-- >>> parseByteRanges "bytes=500-700,601-999"
-- Just [ByteRangeFromTo 500 700,ByteRangeFromTo 601 999]
parseByteRanges :: B.ByteString -> Maybe ByteRanges
parseByteRanges bs1 = do
    bs2 <- stripPrefixB "bytes=" bs1
    (r, bs3) <- range bs2
    ranges (r:) bs3
  where
    range bs2 = do
        (i, bs3) <- B8.readInteger bs2
        if i < 0 -- has prefix "-" ("-0" is not valid, but here treated as "0-")
            then Just (ByteRangeSuffix (negate i), bs3)
            else do
                bs4 <- stripPrefixB "-" bs3
                case B8.readInteger bs4 of
                    Just (j, bs5) | j >= i -> Just (ByteRangeFromTo i j, bs5)
                    _ -> Just (ByteRangeFrom i, bs4)
    ranges front bs3
        | B.null bs3 = Just (front [])
        | otherwise = do
            bs4 <- stripPrefixB "," bs3
            (r, bs5) <- range bs4
            ranges (front . (r:)) bs5

    stripPrefixB x y
        | x `B.isPrefixOf` y = Just (B.drop (B.length x) y)
        | otherwise = Nothing
