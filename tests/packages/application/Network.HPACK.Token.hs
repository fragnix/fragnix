{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/Token.hs" #-}
{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Network.HPACK.Token (
  -- * Data type
    Token(..)
  , tokenIx
  , tokenCIKey
  , tokenFoldedKey
  , toToken
  -- * Ix
  , minTokenIx
  , maxStaticTokenIx
  , maxTokenIx
  , cookieTokenIx
  -- * Utilities
  , isMaxTokenIx
  , isCookieTokenIx
  , isStaticTokenIx
  , isStaticToken
  -- * Defined tokens
  , tokenAuthority
  , tokenMethod
  , tokenPath
  , tokenScheme
  , tokenStatus
  , tokenAcceptCharset
  , tokenAcceptEncoding
  , tokenAcceptLanguage
  , tokenAcceptRanges
  , tokenAccept
  , tokenAccessControlAllowOrigin
  , tokenAge
  , tokenAllow
  , tokenAuthorization
  , tokenCacheControl
  , tokenContentDisposition
  , tokenContentEncoding
  , tokenContentLanguage
  , tokenContentLength
  , tokenContentLocation
  , tokenContentRange
  , tokenContentType
  , tokenCookie
  , tokenDate
  , tokenEtag
  , tokenExpect
  , tokenExpires
  , tokenFrom
  , tokenHost
  , tokenIfMatch
  , tokenIfModifiedSince
  , tokenIfNoneMatch
  , tokenIfRange
  , tokenIfUnmodifiedSince
  , tokenLastModified
  , tokenLink
  , tokenLocation
  , tokenMaxForwards
  , tokenProxyAuthenticate
  , tokenProxyAuthorization
  , tokenRange
  , tokenReferer
  , tokenRefresh
  , tokenRetryAfter
  , tokenServer
  , tokenSetCookie
  , tokenStrictTransportSecurity
  , tokenTransferEncoding
  , tokenUserAgent
  , tokenVary
  , tokenVia
  , tokenWwwAuthenticate
  , tokenConnection
  , tokenTE
  , tokenMax
  ) where

import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), memcmp)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.CaseInsensitive (original, mk, CI(..))

-- $setup
-- >>> :set -XOverloadedStrings

-- | Internal representation for header keys.
data Token = Token {
    ix :: !Int               -- ^ Index for value table
  , shouldBeIndexed :: !Bool -- ^ should be indexed in HPACK
  , isPseudo :: !Bool        -- ^ is this a pseudo header key?
  , tokenKey :: !(CI ByteString) -- ^ Case insensitive header key
  } deriving (Eq, Show)

-- | Extracting an index from a token.
{-# INLINE tokenIx #-}
tokenIx :: Token -> Int
tokenIx (Token n _ _ _) = n

-- | Extracting a case insensitive header key from a token.
{-# INLINE tokenCIKey #-}
tokenCIKey :: Token -> ByteString
tokenCIKey (Token _ _ _ ci) = original ci

-- | Extracting a folded header key from a token.
{-# INLINE tokenFoldedKey #-}
tokenFoldedKey :: Token -> ByteString
tokenFoldedKey (Token _ _ _ ci) = foldedCase ci

tokenAuthority                :: Token
tokenMethod                   :: Token
tokenPath                     :: Token
tokenScheme                   :: Token
tokenStatus                   :: Token
tokenAcceptCharset            :: Token
tokenAcceptEncoding           :: Token
tokenAcceptLanguage           :: Token
tokenAcceptRanges             :: Token
tokenAccept                   :: Token
tokenAccessControlAllowOrigin :: Token
tokenAge                      :: Token
tokenAllow                    :: Token
tokenAuthorization            :: Token
tokenCacheControl             :: Token
tokenContentDisposition       :: Token
tokenContentEncoding          :: Token
tokenContentLanguage          :: Token
tokenContentLength            :: Token
tokenContentLocation          :: Token
tokenContentRange             :: Token
tokenContentType              :: Token
tokenCookie                   :: Token
tokenDate                     :: Token
tokenEtag                     :: Token
tokenExpect                   :: Token
tokenExpires                  :: Token
tokenFrom                     :: Token
tokenHost                     :: Token
tokenIfMatch                  :: Token
tokenIfModifiedSince          :: Token
tokenIfNoneMatch              :: Token
tokenIfRange                  :: Token
tokenIfUnmodifiedSince        :: Token
tokenLastModified             :: Token
tokenLink                     :: Token
tokenLocation                 :: Token
tokenMaxForwards              :: Token
tokenProxyAuthenticate        :: Token
tokenProxyAuthorization       :: Token
tokenRange                    :: Token
tokenReferer                  :: Token
tokenRefresh                  :: Token
tokenRetryAfter               :: Token
tokenServer                   :: Token
tokenSetCookie                :: Token
tokenStrictTransportSecurity  :: Token
tokenTransferEncoding         :: Token
tokenUserAgent                :: Token
tokenVary                     :: Token
tokenVia                      :: Token
tokenWwwAuthenticate          :: Token
tokenConnection               :: Token -- Original
tokenTE                       :: Token -- Original
tokenMax                      :: Token -- Other tokens

tokenAuthority                = Token  0  True  True ":authority"
tokenMethod                   = Token  1  True  True ":method"
tokenPath                     = Token  2 False  True ":path"
tokenScheme                   = Token  3  True  True ":scheme"
tokenStatus                   = Token  4  True  True ":status"
tokenAcceptCharset            = Token  5  True False "Accept-Charset"
tokenAcceptEncoding           = Token  6  True False "Accept-Encoding"
tokenAcceptLanguage           = Token  7  True False "Accept-Language"
tokenAcceptRanges             = Token  8  True False "Accept-Ranges"
tokenAccept                   = Token  9  True False "Accept"
tokenAccessControlAllowOrigin = Token 10  True False "Access-Control-Allow-Origin"
tokenAge                      = Token 11  True False "Age"
tokenAllow                    = Token 12  True False "Allow"
tokenAuthorization            = Token 13  True False "Authorization"
tokenCacheControl             = Token 14  True False "Cache-Control"
tokenContentDisposition       = Token 15  True False "Content-Disposition"
tokenContentEncoding          = Token 16  True False "Content-Encoding"
tokenContentLanguage          = Token 17  True False "Content-Language"
tokenContentLength            = Token 18 False False "Content-Length"
tokenContentLocation          = Token 19 False False "Content-Location"
tokenContentRange             = Token 20  True False "Content-Range"
tokenContentType              = Token 21  True False "Content-Type"
tokenCookie                   = Token 22  True False "Cookie"
tokenDate                     = Token 23  True False "Date"
tokenEtag                     = Token 24 False False "Etag"
tokenExpect                   = Token 25  True False "Expect"
tokenExpires                  = Token 26  True False "Expires"
tokenFrom                     = Token 27  True False "From"
tokenHost                     = Token 28  True False "Host"
tokenIfMatch                  = Token 29  True False "If-Match"
tokenIfModifiedSince          = Token 30  True False "If-Modified-Since"
tokenIfNoneMatch              = Token 31  True False "If-None-Match"
tokenIfRange                  = Token 32  True False "If-Range"
tokenIfUnmodifiedSince        = Token 33  True False "If-Unmodified-Since"
tokenLastModified             = Token 34  True False "Last-Modified"
tokenLink                     = Token 35  True False "Link"
tokenLocation                 = Token 36  True False "Location"
tokenMaxForwards              = Token 37  True False "Max-Forwards"
tokenProxyAuthenticate        = Token 38  True False "Proxy-Authenticate"
tokenProxyAuthorization       = Token 39  True False "Proxy-Authorization"
tokenRange                    = Token 40  True False "Range"
tokenReferer                  = Token 41  True False "Referer"
tokenRefresh                  = Token 42  True False "Refresh"
tokenRetryAfter               = Token 43  True False "Retry-After"
tokenServer                   = Token 44  True False "Server"
tokenSetCookie                = Token 45 False False "Set-Cookie"
tokenStrictTransportSecurity  = Token 46  True False "Strict-Transport-Security"
tokenTransferEncoding         = Token 47  True False "Transfer-Encoding"
tokenUserAgent                = Token 48  True False "User-Agent"
tokenVary                     = Token 49  True False "Vary"
tokenVia                      = Token 50  True False "Via"
tokenWwwAuthenticate          = Token 51  True False "Www-Authenticate"
-- | Not defined in the static table.
tokenConnection               = Token 52 False False "Connection"
-- | Not defined in the static table.
tokenTE                       = Token 53 False False "TE"
-- | A place holder to hold header keys not defined in the static table.
tokenMax                      = Token 54  True False "for other tokens"

-- | Minimum token index.
minTokenIx :: Int
minTokenIx = 0

-- | Maximun token index defined in the static table.
maxStaticTokenIx :: Int
maxStaticTokenIx = 51

-- | Maximum token index.
maxTokenIx :: Int
maxTokenIx = 54

-- | Token index for 'tokenCookie'.
cookieTokenIx :: Int
cookieTokenIx = 22

-- | Is this token ix for Cookie?
{-# INLINE isCookieTokenIx #-}
isCookieTokenIx :: Int -> Bool
isCookieTokenIx n = n == cookieTokenIx

-- | Is this token ix to be held in the place holder?
{-# INLINE isMaxTokenIx #-}
isMaxTokenIx :: Int -> Bool
isMaxTokenIx n = n == maxTokenIx

-- | Is this token ix for a header not defined in the static table?
{-# INLINE isStaticTokenIx #-}
isStaticTokenIx :: Int -> Bool
isStaticTokenIx n = n <= maxStaticTokenIx

-- | Is this token for a header not defined in the static table?
{-# INLINE isStaticToken #-}
isStaticToken :: Token -> Bool
isStaticToken n = tokenIx n <= maxStaticTokenIx

-- | Making a token from a header key.
--
-- >>> toToken ":authority" == tokenAuthority
-- True
-- >>> toToken "foo"
-- Token {ix = 54, shouldBeIndexed = True, isPseudo = False, tokenKey = "foo"}
-- >>> toToken ":bar"
-- Token {ix = 54, shouldBeIndexed = True, isPseudo = True, tokenKey = ":bar"}
toToken :: ByteString -> Token
toToken bs = case len of
    2 -> if bs === "te" then tokenTE else mkTokenMax bs
    3 -> case lst of
        97  | bs === "via" -> tokenVia
        101 | bs === "age" -> tokenAge
        _                  -> mkTokenMax bs
    4 -> case lst of
        101 | bs === "date" -> tokenDate
        103 | bs === "etag" -> tokenEtag
        107 | bs === "link" -> tokenLink
        109 | bs === "from" -> tokenFrom
        116 | bs === "host" -> tokenHost
        121 | bs === "vary" -> tokenVary
        _                   -> mkTokenMax bs
    5 -> case lst of
        101 | bs === "range" -> tokenRange
        104 | bs === ":path" -> tokenPath
        119 | bs === "allow" -> tokenAllow
        _                    -> mkTokenMax bs
    6 -> case lst of
        101 | bs === "cookie" -> tokenCookie
        114 | bs === "server" -> tokenServer
        116 | bs === "expect" -> tokenExpect
            | bs === "accept" -> tokenAccept
        _                     -> mkTokenMax bs
    7 -> case lst of
        100 | bs === ":method" -> tokenMethod
        101 | bs === ":scheme" -> tokenScheme
        104 | bs === "refresh" -> tokenRefresh
        114 | bs === "referer" -> tokenReferer
        115 | bs === "expires" -> tokenExpires
            | bs === ":status" -> tokenStatus
        _                      -> mkTokenMax bs
    8 -> case lst of
        101 | bs === "if-range" -> tokenIfRange
        104 | bs === "if-match" -> tokenIfMatch
        110 | bs === "location" -> tokenLocation
        _                       -> mkTokenMax bs
    10 -> case lst of
        101 | bs === "set-cookie" -> tokenSetCookie
        110 | bs === "connection" -> tokenConnection
        116 | bs === "user-agent" -> tokenUserAgent
        121 | bs === ":authority" -> tokenAuthority
        _                         -> mkTokenMax bs
    11 -> case lst of
        114 | bs === "retry-after" -> tokenRetryAfter
        _                          -> mkTokenMax bs
    12 -> case lst of
        101 | bs === "content-type" -> tokenContentType
        115 | bs === "max-forwards" -> tokenMaxForwards
        _                           -> mkTokenMax bs
    13 -> case lst of
        100 | bs === "last-modified" -> tokenLastModified
        101 | bs === "content-range" -> tokenContentRange
        104 | bs === "if-none-match" -> tokenIfNoneMatch
        108 | bs === "cache-control" -> tokenCacheControl
        110 | bs === "authorization" -> tokenAuthorization
        115 | bs === "accept-ranges" -> tokenAcceptRanges
        _                            -> mkTokenMax bs
    14 -> case lst of
        104 | bs === "content-length" -> tokenContentLength
        116 | bs === "accept-charset" -> tokenAcceptCharset
        _                             -> mkTokenMax bs
    15 -> case lst of
        101 | bs === "accept-language" -> tokenAcceptLanguage
        103 | bs === "accept-encoding" -> tokenAcceptEncoding
        _                              -> mkTokenMax bs
    16 -> case lst of
        101 | bs === "content-language" -> tokenContentLanguage
            | bs === "www-authenticate" -> tokenWwwAuthenticate
        103 | bs === "content-encoding" -> tokenContentEncoding
        110 | bs === "content-location" -> tokenContentLocation
        _                               -> mkTokenMax bs
    17 -> case lst of
        101 | bs === "if-modified-since" -> tokenIfModifiedSince
        103 | bs === "transfer-encoding" -> tokenTransferEncoding
        _                                -> mkTokenMax bs
    18 -> case lst of
        101 | bs === "proxy-authenticate" -> tokenProxyAuthenticate
        _                                 -> mkTokenMax bs
    19 -> case lst of
        101 | bs === "if-unmodified-since" -> tokenIfUnmodifiedSince
        110 | bs === "proxy-authorization" -> tokenProxyAuthorization
            | bs === "content-disposition" -> tokenContentDisposition
        _                                  -> mkTokenMax bs
    25 -> case lst of
        121 | bs === "strict-transport-security" -> tokenStrictTransportSecurity
        _                                        -> mkTokenMax bs
    27 -> case lst of
        110 | bs === "access-control-allow-origin" -> tokenAccessControlAllowOrigin
        _                                          -> mkTokenMax bs
    _  -> mkTokenMax bs
  where
    len = B.length bs
    lst = B.last bs
    PS fp1 off1 siz === PS fp2 off2 _ = unsafeDupablePerformIO $
      withForeignPtr fp1 $ \p1 ->
      withForeignPtr fp2 $ \p2 -> do
        i <- memcmp (p1 `plusPtr` off1) (p2 `plusPtr` off2) siz
        return $! i == 0

mkTokenMax :: ByteString -> Token
mkTokenMax bs = Token maxTokenIx True p (mk bs)
  where
    !p | B.length bs == 0 = False
       | B.head bs == 58  = True
       | otherwise        = False
