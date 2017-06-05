{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Web/Cookie.hs" #-}



































































{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Cookie
    ( -- * Server to client
      -- ** Data type
      SetCookie
    , setCookieName
    , setCookieValue
    , setCookiePath
    , setCookieExpires
    , setCookieMaxAge
    , setCookieDomain
    , setCookieHttpOnly
    , setCookieSecure
    , setCookieSameSite
    , SameSiteOption
    , sameSiteLax
    , sameSiteStrict
      -- ** Functions
    , parseSetCookie
    , renderSetCookie
    , def
      -- * Client to server
    , Cookies
    , parseCookies
    , renderCookies
      -- ** UTF8 Version
    , CookiesText
    , parseCookiesText
    , renderCookiesText
      -- * Expires field
    , expiresFormat
    , formatCookieExpires
    , parseCookieExpires
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char (toLower)
import Blaze.ByteString.Builder (Builder, fromByteString, copyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.Monoid (mempty, mappend, mconcat)
import Data.Word (Word8)
import Data.Ratio (numerator, denominator)
import Data.Time (UTCTime (UTCTime), toGregorian, fromGregorian, formatTime, parseTime)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time (defaultTimeLocale)
import Control.Arrow (first)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Control.Arrow ((***))
import Data.Maybe (isJust)
import Data.Default.Class (Default (def))
import Control.DeepSeq (NFData (rnf))

-- | Textual cookies. Functions assume UTF8 encoding.
type CookiesText = [(Text, Text)]

parseCookiesText :: S.ByteString -> CookiesText
parseCookiesText =
    map (go *** go) . parseCookies
  where
    go = decodeUtf8With lenientDecode

-- FIXME to speed things up, skip encodeUtf8 and use fromText instead
renderCookiesText :: CookiesText -> Builder
renderCookiesText = renderCookies . map (encodeUtf8 *** encodeUtf8)

type Cookies = [(S.ByteString, S.ByteString)]

-- | Decode the value of a \"Cookie\" request header into key/value pairs.
parseCookies :: S.ByteString -> Cookies
parseCookies s
  | S.null s = []
  | otherwise =
    let (x, y) = breakDiscard 59 s -- semicolon
     in parseCookie x : parseCookies y

parseCookie :: S.ByteString -> (S.ByteString, S.ByteString)
parseCookie s =
    let (key, value) = breakDiscard 61 s -- equals sign
        key' = S.dropWhile (== 32) key -- space
     in (key', value)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.breakByte w s
     in (x, S.drop 1 y)

renderCookies :: Cookies -> Builder
renderCookies [] = mempty
renderCookies cs =
    foldr1 go $ map renderCookie cs
  where
    go x y = x `mappend` fromChar ';' `mappend` y

renderCookie :: (S.ByteString, S.ByteString) -> Builder
renderCookie (k, v) = fromByteString k `mappend` fromChar '='
                                       `mappend` fromByteString v
-- | Data type representing the key-value pair to use for a cookie, as well as configuration options for it.
--
-- ==== Creating a SetCookie
--
-- 'SetCookie' does not export a constructor; instead, use the 'Default' instance to create one and override values (see <http://www.yesodweb.com/book/settings-types> for details):
--
-- @
-- import Web.Cookie
-- :set -XOverloadedStrings
-- let cookie = 'def' { 'setCookieName' = "cookieName", 'setCookieValue' = "cookieValue" }
-- @
--
-- ==== Cookie Configuration
--
-- Cookies have several configuration options; a brief summary of each option is given below. For more information, see <http://tools.ietf.org/html/rfc6265#section-4.1.2 RFC 6265> or <https://en.wikipedia.org/wiki/HTTP_cookie#Cookie_attributes Wikipedia>.
data SetCookie = SetCookie
    { setCookieName :: S.ByteString -- ^ The name of the cookie. Default value: @"name"@
    , setCookieValue :: S.ByteString -- ^ The value of the cookie. Default value: @"value"@
    , setCookiePath :: Maybe S.ByteString -- ^ The URL path for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the path of the request that sets the cookie).
    , setCookieExpires :: Maybe UTCTime -- ^ The time at which to expire the cookie. Default value: @Nothing@ (The browser will default to expiring a cookie when the browser is closed).
    , setCookieMaxAge :: Maybe DiffTime -- ^ The maximum time to keep the cookie, in seconds. Default value: @Nothing@ (The browser defaults to expiring a cookie when the browser is closed).
    , setCookieDomain :: Maybe S.ByteString -- ^ The domain for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the current domain).
    , setCookieHttpOnly :: Bool -- ^ Marks the cookie as "HTTP only", i.e. not accessible from Javascript. Default value: @False@
    , setCookieSecure :: Bool -- ^ Instructs the browser to only send the cookie over HTTPS. Default value: @False@
    , setCookieSameSite :: Maybe SameSiteOption -- ^ Marks the cookie as "same site", i.e. should not be sent with cross-site requests. Default value: @Nothing@
    }
    deriving (Eq, Show)

-- | Data type representing the options for a SameSite cookie
data SameSiteOption = Lax | Strict deriving (Show, Eq)

instance NFData SameSiteOption where
  rnf x = x `seq` ()

sameSiteLax :: SameSiteOption
sameSiteLax = Lax

sameSiteStrict :: SameSiteOption
sameSiteStrict = Strict

instance NFData SetCookie where
    rnf (SetCookie a b c d e f g h i) =
        a `seq`
        b `seq`
        rnfMBS c `seq`
        rnf d `seq`
        rnf e `seq`
        rnfMBS f `seq`
        rnf g `seq`
        rnf h `seq`
        rnf i
      where
        -- For backwards compatibility
        rnfMBS Nothing = ()
        rnfMBS (Just bs) = bs `seq` ()

instance Default SetCookie where
    def = SetCookie
        { setCookieName     = "name"
        , setCookieValue    = "value"
        , setCookiePath     = Nothing
        , setCookieExpires  = Nothing
        , setCookieMaxAge   = Nothing
        , setCookieDomain   = Nothing
        , setCookieHttpOnly = False
        , setCookieSecure   = False
        , setCookieSameSite = Nothing
        }

renderSetCookie :: SetCookie -> Builder
renderSetCookie sc = mconcat
    [ fromByteString (setCookieName sc)
    , fromChar '='
    , fromByteString (setCookieValue sc)
    , case setCookiePath sc of
        Nothing -> mempty
        Just path -> copyByteString "; Path="
                     `mappend` fromByteString path
    , case setCookieExpires sc of
        Nothing -> mempty
        Just e -> copyByteString "; Expires=" `mappend`
                  fromByteString (formatCookieExpires e)
    , case setCookieMaxAge sc of
        Nothing -> mempty
        Just ma -> copyByteString"; Max-Age=" `mappend`
                   fromByteString (formatCookieMaxAge ma)
    , case setCookieDomain sc of
        Nothing -> mempty
        Just d -> copyByteString "; Domain=" `mappend`
                  fromByteString d
    , if setCookieHttpOnly sc
        then copyByteString "; HttpOnly"
        else mempty
    , if setCookieSecure sc
        then copyByteString "; Secure"
        else mempty
    , case setCookieSameSite sc of
        Nothing -> mempty
        Just Lax -> copyByteString "; SameSite=Lax"
        Just Strict -> copyByteString "; SameSite=Strict"
    ]

parseSetCookie :: S.ByteString -> SetCookie
parseSetCookie a = SetCookie
    { setCookieName = name
    , setCookieValue = value
    , setCookiePath = lookup "path" flags
    , setCookieExpires =
        lookup "expires" flags >>= parseCookieExpires
    , setCookieMaxAge =
        lookup "max-age" flags >>= parseCookieMaxAge
    , setCookieDomain = lookup "domain" flags
    , setCookieHttpOnly = isJust $ lookup "httponly" flags
    , setCookieSecure = isJust $ lookup "secure" flags
    , setCookieSameSite = case lookup "samesite" flags of
        Just "Lax" -> Just Lax
        Just "Strict" -> Just Strict
        _ -> Nothing
    }
  where
    pairs = map (parsePair . dropSpace) $ S.split 59 a ++ [S8.empty] -- 59 = semicolon
    (name, value) = head pairs
    flags = map (first (S8.map toLower)) $ tail pairs
    parsePair = breakDiscard 61 -- equals sign
    dropSpace = S.dropWhile (== 32) -- space

expiresFormat :: String
expiresFormat = "%a, %d-%b-%Y %X GMT"

-- | Format a 'UTCTime' for a cookie.
formatCookieExpires :: UTCTime -> S.ByteString
formatCookieExpires =
    S8.pack . formatTime defaultTimeLocale expiresFormat

parseCookieExpires :: S.ByteString -> Maybe UTCTime
parseCookieExpires =
    fmap fuzzYear . parseTime defaultTimeLocale expiresFormat . S8.unpack
  where
    -- See: https://github.com/snoyberg/cookie/issues/5
    fuzzYear orig@(UTCTime day diff)
        | x >= 70 && x <= 99 = addYear 1900
        | x >= 0 && x <= 69 = addYear 2000
        | otherwise = orig
      where
        (x, y, z) = toGregorian day
        addYear x' = UTCTime (fromGregorian (x + x') y z) diff

-- | Format a 'DiffTime' for a cookie.
formatCookieMaxAge :: DiffTime -> S.ByteString
formatCookieMaxAge difftime = S8.pack $ show (num `div` denom)
  where rational = toRational difftime
        num = numerator rational
        denom = denominator rational

parseCookieMaxAge :: S.ByteString -> Maybe DiffTime
parseCookieMaxAge bs
  | all (\ c -> c >= '0' && c <= '9') $ unpacked = Just $ secondsToDiffTime $ read unpacked
  | otherwise = Nothing
  where unpacked = S8.unpack bs
