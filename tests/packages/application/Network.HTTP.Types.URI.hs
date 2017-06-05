{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/HTTP/Types/URI.hs" #-}





























































{-# LANGUAGE OverloadedStrings, CPP #-}
module Network.HTTP.Types.URI
(
  -- * Query string
  QueryItem
, Query
, SimpleQueryItem
, SimpleQuery
, simpleQueryToQuery
, renderQuery
, renderQueryBuilder
, renderSimpleQuery
, parseQuery
, parseSimpleQuery
  -- ** Text query string (UTF8 encoded)
, QueryText
, queryTextToQuery
, queryToQueryText
, renderQueryText
, parseQueryText
  -- * Path segments
, encodePathSegments
, decodePathSegments
, encodePathSegmentsRelative
  -- * Path (segments + query string)
, extractPath
, encodePath
, decodePath
  -- * URL encoding / decoding
, urlEncodeBuilder
, urlEncode
, urlDecode
)
where

import           Control.Arrow
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Text                      (Text)
import           Data.Text.Encoding             (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error       (lenientDecode)
import           Data.Word
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Data.ByteString                as B
import           Data.ByteString.Char8          () {-IsString-}

-- | Query item
type QueryItem = (B.ByteString, Maybe B.ByteString)

-- | Query.
-- 
-- General form: a=b&c=d, but if the value is Nothing, it becomes
-- a&c=d.
type Query = [QueryItem]

-- | Like Query, but with 'Text' instead of 'B.ByteString' (UTF8-encoded).
type QueryText = [(Text, Maybe Text)]

-- | Convert 'QueryText' to 'Query'.
queryTextToQuery :: QueryText -> Query
queryTextToQuery = map $ encodeUtf8 *** fmap encodeUtf8

-- | Convert 'QueryText' to a 'Blaze.Builder'.
renderQueryText :: Bool -- ^ prepend a question mark?
                -> QueryText
                -> Blaze.Builder
renderQueryText b = renderQueryBuilder b . queryTextToQuery

-- | Convert 'Query' to 'QueryText' (leniently decoding the UTF-8).
queryToQueryText :: Query -> QueryText
queryToQueryText =
    map $ go *** fmap go
  where
    go = decodeUtf8With lenientDecode

-- | Parse 'QueryText' from a 'B.ByteString'. See 'parseQuery' for details.
parseQueryText :: B.ByteString -> QueryText
parseQueryText = queryToQueryText . parseQuery

-- | Simplified Query item type without support for parameter-less items.
type SimpleQueryItem = (B.ByteString, B.ByteString)

-- | Simplified Query type without support for parameter-less items.
type SimpleQuery = [SimpleQueryItem]

-- | Convert 'SimpleQuery' to 'Query'.
simpleQueryToQuery :: SimpleQuery -> Query
simpleQueryToQuery = map (\(a, b) -> (a, Just b))

-- | Convert 'Query' to a 'Builder'.
renderQueryBuilder :: Bool -- ^ prepend a question mark?
                   -> Query
                   -> Blaze.Builder
renderQueryBuilder _ [] = mempty
-- FIXME replace mconcat + map with foldr
renderQueryBuilder qmark' (p:ps) = mconcat
    $ go (if qmark' then qmark else mempty) p
    : map (go amp) ps
  where
    qmark = Blaze.copyByteString "?"
    amp = Blaze.copyByteString "&"
    equal = Blaze.copyByteString "="
    go sep (k, mv) = mconcat [
                      sep
                     , urlEncodeBuilder True k
                     , case mv of
                         Nothing -> mempty
                         Just v -> equal `mappend` urlEncodeBuilder True v
                     ]

-- | Convert 'Query' to 'ByteString'.
renderQuery :: Bool -- ^ prepend question mark?
            -> Query -> B.ByteString
renderQuery qm = Blaze.toByteString . renderQueryBuilder qm

-- | Convert 'SimpleQuery' to 'ByteString'.
renderSimpleQuery :: Bool -- ^ prepend question mark?
                  -> SimpleQuery -> B.ByteString
renderSimpleQuery useQuestionMark = renderQuery useQuestionMark . simpleQueryToQuery

-- | Split out the query string into a list of keys and values. A few
-- importants points:
-- 
-- * The result returned is still bytestrings, since we perform no character
-- decoding here. Most likely, you will want to use UTF-8 decoding, but this is
-- left to the user of the library.
-- 
-- * Percent decoding errors are ignored. In particular, "%Q" will be output as
-- "%Q".
parseQuery :: B.ByteString -> Query
parseQuery = parseQueryString' . dropQuestion
  where
    dropQuestion q =
        case B.uncons q of
            Just (63, q') -> q'
            _ -> q
    parseQueryString' q | B.null q = []
    parseQueryString' q =
        let (x, xs) = breakDiscard queryStringSeparators q
         in parsePair x : parseQueryString' xs
      where
        parsePair x =
            let (k, v) = B.breakByte 61 x -- equal sign
                v'' =
                    case B.uncons v of
                        Just (_, v') -> Just $ urlDecode True v'
                        _ -> Nothing
             in (urlDecode True k, v'')

queryStringSeparators :: B.ByteString
queryStringSeparators = B.pack [38,59] -- ampersand, semicolon

-- | Break the second bytestring at the first occurence of any bytes from
-- the first bytestring, discarding that byte.
breakDiscard :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
breakDiscard seps s =
    let (x, y) = B.break (`B.elem` seps) s
     in (x, B.drop 1 y)

-- | Parse 'SimpleQuery' from a 'ByteString'.
parseSimpleQuery :: B.ByteString -> SimpleQuery
parseSimpleQuery = map (second $ fromMaybe B.empty) . parseQuery

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

unreservedQS, unreservedPI :: [Word8]
unreservedQS = map ord8 "-_.~"
unreservedPI = map ord8 "-_.~:@&=+$,"

-- | Percent-encoding for URLs.
urlEncodeBuilder' :: [Word8] -> B.ByteString -> Blaze.Builder
urlEncodeBuilder' extraUnreserved = mconcat . map encodeChar . B.unpack
    where
      encodeChar ch | unreserved ch = Blaze.fromWord8 ch
                    | otherwise     = h2 ch
      
      unreserved ch | ch >= 65 && ch <= 90  = True -- A-Z
                    | ch >= 97 && ch <= 122 = True -- a-z
                    | ch >= 48 && ch <= 57  = True -- 0-9
      unreserved c = c `elem` extraUnreserved
      
      h2 v = let (a, b) = v `divMod` 16 in Blaze.fromWord8s [37, h a, h b] -- percent (%)
      h i | i < 10    = 48 + i -- zero (0)
          | otherwise = 65 + i - 10 -- 65: A

-- | Percent-encoding for URLs (using 'Blaze.Builder').
urlEncodeBuilder
    :: Bool -- ^ Whether input is in query string. True: Query string, False: Path element
    -> B.ByteString
    -> Blaze.Builder
urlEncodeBuilder True  = urlEncodeBuilder' unreservedQS
urlEncodeBuilder False = urlEncodeBuilder' unreservedPI

-- | Percent-encoding for URLs.
urlEncode :: Bool -- ^ Whether to decode '+' to ' '
          -> B.ByteString -- ^ The ByteString to encode as URL
          -> B.ByteString -- ^ The encoded URL
urlEncode q = Blaze.toByteString . urlEncodeBuilder q

-- | Percent-decoding.
urlDecode :: Bool -- ^ Whether to decode '+' to ' '
          -> B.ByteString -> B.ByteString
urlDecode replacePlus z = fst $ B.unfoldrN (B.length z) go z
  where
    go bs =
        case B.uncons bs of
            Nothing -> Nothing
            Just (43, ws) | replacePlus -> Just (32, ws) -- plus to space
            Just (37, ws) -> Just $ fromMaybe (37, ws) $ do -- percent
                (x, xs) <- B.uncons ws
                x' <- hexVal x
                (y, ys) <- B.uncons xs
                y' <- hexVal y
                Just (combine x' y', ys)
            Just (w, ws) -> Just (w, ws)
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

-- | Encodes a list of path segments into a valid URL fragment.
-- 
-- This function takes the following three steps:
-- 
-- * UTF-8 encodes the characters.
-- 
-- * Performs percent encoding on all unreserved characters, as well as \:\@\=\+\$,
-- 
-- * Prepends each segment with a slash.
-- 
-- For example:
-- 
-- > encodePathSegments [\"foo\", \"bar\", \"baz\"]
-- 
-- \"\/foo\/bar\/baz\"
-- 
-- > encodePathSegments [\"foo bar\", \"baz\/bin\"]
-- 
-- \"\/foo\%20bar\/baz\%2Fbin\"
-- 
-- > encodePathSegments [\"שלום\"]
-- 
-- \"\/%D7%A9%D7%9C%D7%95%D7%9D\"
-- 
-- Huge thanks to Jeremy Shaw who created the original implementation of this
-- function in web-routes and did such thorough research to determine all
-- correct escaping procedures.
encodePathSegments :: [Text] -> Blaze.Builder
encodePathSegments [] = mempty
encodePathSegments (x:xs) =
    Blaze.copyByteString "/"
    `mappend` encodePathSegment x
    `mappend` encodePathSegments xs

-- | Like encodePathSegments, but without the initial slash.
encodePathSegmentsRelative :: [Text] -> Blaze.Builder
encodePathSegmentsRelative xs = mconcat $ intersperse (Blaze.copyByteString "/") (map encodePathSegment xs)

encodePathSegment :: Text -> Blaze.Builder
encodePathSegment = urlEncodeBuilder False . encodeUtf8

-- | Parse a list of path segments from a valid URL fragment.
decodePathSegments :: B.ByteString -> [Text]
decodePathSegments "" = []
decodePathSegments "/" = []
decodePathSegments a =
    go $ drop1Slash a
  where
    drop1Slash bs =
        case B.uncons bs of
            Just (47, bs') -> bs' -- 47 == /
            _ -> bs
    go bs =
        let (x, y) = B.breakByte 47 bs
         in decodePathSegment x :
            if B.null y
                then []
                else go $ B.drop 1 y

decodePathSegment :: B.ByteString -> Text
decodePathSegment = decodeUtf8With lenientDecode . urlDecode False

-- | Extract whole path (path segments + query) from a
-- <http://tools.ietf.org/html/rfc2616#section-5.1.2 RFC 2616 Request-URI>.
--
-- >>> extractPath "/path"
-- "/path"
--
-- >>> extractPath "http://example.com:8080/path"
-- "/path"
--
-- >>> extractPath "http://example.com"
-- "/"
--
-- >>> extractPath ""
-- "/"
extractPath :: B.ByteString -> B.ByteString
extractPath = ensureNonEmpty . extract
  where
    extract path
      | "http://"  `B.isPrefixOf` path = (snd . breakOnSlash . B.drop 7) path
      | "https://" `B.isPrefixOf` path = (snd . breakOnSlash . B.drop 8) path
      | otherwise                      = path
    breakOnSlash = B.breakByte 47
    ensureNonEmpty "" = "/"
    ensureNonEmpty p  = p

-- | Encode a whole path (path segments + query).
encodePath :: [Text] -> Query -> Blaze.Builder
encodePath x [] = encodePathSegments x
encodePath x y = encodePathSegments x `mappend` renderQueryBuilder True y

-- | Decode a whole path (path segments + query).
decodePath :: B.ByteString -> ([Text], Query)
decodePath b =
    let (x, y) = B.breakByte 63 b -- question mark
    in (decodePathSegments x, parseQuery y)
