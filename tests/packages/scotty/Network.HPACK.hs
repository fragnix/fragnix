{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK.hs" #-}



































































{-# LANGUAGE CPP #-}

-- | HPACK(<https://tools.ietf.org/html/rfc7541>) encoding and decoding a header list.
module Network.HPACK (
  -- * Encoding and decoding
    encodeHeader
  , decodeHeader
  -- * Encoding and decoding with token
  , encodeTokenHeader
  , decodeTokenHeader
  -- * DynamicTable
  , DynamicTable
  , defaultDynamicTableSize
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , clearDynamicTable
  , withDynamicTableForEncoding
  , withDynamicTableForDecoding
  , setLimitForEncoding
  -- * Strategy for encoding
  , CompressionAlgo(..)
  , EncodeStrategy(..)
  , defaultEncodeStrategy
  -- * Errors
  , DecodeError(..)
  , BufferOverrun(..)
  -- * Headers
  , HeaderList
  , Header
  , HeaderName
  , HeaderValue
  , TokenHeaderList
  , TokenHeader
  -- * Value table
  , ValueTable
  , getHeaderValue
  , toHeaderTable
  -- * Basic types
  , Size
  , Index
  , Buffer
  , BufferSize
  ) where

import Network.HPACK.HeaderBlock
import Network.HPACK.Table
import Network.HPACK.Types

-- | Default dynamic table size.
--   The value is 4,096 bytes: an array has 128 entries.
--
-- >>> defaultDynamicTableSize
-- 4096
defaultDynamicTableSize :: Int
defaultDynamicTableSize = 4096
