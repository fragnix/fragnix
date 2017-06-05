{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/Table/Static.hs" #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HPACK.Table.Static (
    toStaticEntry
  , staticTableSize
  , staticTableList
  ) where

import Data.Array (Array, listArray)
import Data.Array.Base (unsafeAt)
import Network.HPACK.Table.Entry

----------------------------------------------------------------

-- | The size of static table.
staticTableSize :: Size
staticTableSize = length staticTableList

{-# INLINE toStaticEntry #-}
-- | Get 'Entry' from the static table.
--
-- >>> toStaticEntry 1
-- Entry 42 (Token {ix = 0, shouldBeIndexed = True, isPseudo = True, tokenKey = ":authority"}) ""
-- >>> toStaticEntry 8
-- Entry 42 (Token {ix = 4, shouldBeIndexed = True, isPseudo = True, tokenKey = ":status"}) "200"
-- >>> toStaticEntry 50
-- Entry 37 (Token {ix = 40, shouldBeIndexed = True, isPseudo = False, tokenKey = "Range"}) ""
toStaticEntry :: Index -> Entry
toStaticEntry sidx = staticTable `unsafeAt` (sidx - 1)

-- | Pre-defined static table.
staticTable :: Array Index Entry
staticTable = listArray (1,staticTableSize) $ map toEntry staticTableList

----------------------------------------------------------------

staticTableList :: [Header]
staticTableList = [
    (":authority", "")
  , (":method", "GET")
  , (":method", "POST")
  , (":path", "/")
  , (":path", "/index.html")
  , (":scheme", "http")
  , (":scheme", "https")
  , (":status", "200")
  , (":status", "204")
  , (":status", "206")
  , (":status", "304")
  , (":status", "400")
  , (":status", "404")
  , (":status", "500")
  , ("accept-charset", "")
  , ("accept-encoding", "gzip, deflate")
  , ("accept-language", "")
  , ("accept-ranges", "")
  , ("accept", "")
  , ("access-control-allow-origin", "")
  , ("age", "")
  , ("allow", "")
  , ("authorization", "")
  , ("cache-control", "")
  , ("content-disposition", "")
  , ("content-encoding", "")
  , ("content-language", "")
  , ("content-length", "")
  , ("content-location", "")
  , ("content-range", "")
  , ("content-type", "")
  , ("cookie", "")
  , ("date", "")
  , ("etag", "")
  , ("expect", "")
  , ("expires", "")
  , ("from", "")
  , ("host", "")
  , ("if-match", "")
  , ("if-modified-since", "")
  , ("if-none-match", "")
  , ("if-range", "")
  , ("if-unmodified-since", "")
  , ("last-modified", "")
  , ("link", "")
  , ("location", "")
  , ("max-forwards", "")
  , ("proxy-authenticate", "")
  , ("proxy-authorization", "")
  , ("range", "")
  , ("referer", "")
  , ("refresh", "")
  , ("retry-after", "")
  , ("server", "")
  , ("set-cookie", "")
  , ("strict-transport-security", "")
  , ("transfer-encoding", "")
  , ("user-agent", "")
  , ("vary", "")
  , ("via", "")
  , ("www-authenticate", "")
  ]
