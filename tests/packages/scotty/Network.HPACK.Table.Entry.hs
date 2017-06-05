{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/Table/Entry.hs" #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.HPACK.Table.Entry (
  -- * Type
    Size
  , Entry(..)
  , Header      -- re-exporting
  , HeaderName  -- re-exporting
  , HeaderValue -- re-exporting
  , Index       -- re-exporting
  -- * Header and Entry
  , toEntry
  , toEntryToken
  -- * Getters
  , entrySize
  , entryTokenHeader
  , entryToken
  , entryHeaderName
  , entryHeaderValue
  -- * For initialization
  , dummyEntry
  , maxNumbers
  ) where

import qualified Data.ByteString as BS
import Network.HPACK.Token
import Network.HPACK.Types

----------------------------------------------------------------

-- | Size in bytes.
type Size = Int

-- | Type for table entry. Size includes the 32 bytes magic number.
data Entry = Entry Size Token HeaderValue deriving Show

----------------------------------------------------------------

headerSizeMagicNumber :: Size
headerSizeMagicNumber = 32

headerSize :: Header -> Size
headerSize (k,v) = BS.length k
                 + BS.length v
                 + headerSizeMagicNumber

headerSize' :: Token -> HeaderValue -> Size
headerSize' t v = BS.length (tokenFoldedKey t)
                + BS.length v
                + headerSizeMagicNumber

----------------------------------------------------------------

-- | From 'Header' to 'Entry'.
toEntry :: Header -> Entry
toEntry kv@(k,v) = Entry siz t v
  where
    !t = toToken k
    !siz = headerSize kv

toEntryToken :: Token -> HeaderValue -> Entry
toEntryToken t v = Entry siz t v
  where
    !siz = headerSize' t v

----------------------------------------------------------------

-- | Getting the size of 'Entry'.
entrySize :: Entry -> Size
entrySize (Entry siz _ _) = siz

-- | Getting 'TokenHeader'.
entryTokenHeader :: Entry -> TokenHeader
entryTokenHeader (Entry _ t v) = (t, v)

-- | Getting 'Token'.
entryToken :: Entry -> Token
entryToken (Entry _ t _) = t

-- | Getting 'HeaderName'.
entryHeaderName :: Entry -> HeaderName
entryHeaderName (Entry _ t _) = tokenFoldedKey t

-- | Getting 'HeaderValue'.
entryHeaderValue :: Entry -> HeaderValue
entryHeaderValue (Entry _ _ v) = v

----------------------------------------------------------------

-- | Dummy 'Entry' to initialize a dynamic table.
dummyEntry :: Entry
dummyEntry = Entry 0 tokenMax "dummyValue"

-- | How many entries can be stored in a dynamic table?
maxNumbers :: Size -> Int
maxNumbers siz = siz `div` headerSizeMagicNumber
