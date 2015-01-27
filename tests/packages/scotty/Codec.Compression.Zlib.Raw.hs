{-# LINE 1 "Codec/Compression/Zlib/Raw.hs" #-}
# 1 "Codec/Compression/Zlib/Raw.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1






















































































# 8 "<command-line>" 2
# 1 "Codec/Compression/Zlib/Raw.hs"
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2008 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Compression and decompression of data streams in the raw deflate format.
--
-- The format is described in detail in RFC #1951:
-- <http://www.ietf.org/rfc/rfc1951.txt>
--
-- See also the zlib home page: <http://zlib.net/>
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Raw (
  
  -- * Simple compression and decompression
  compress,
  decompress,

  -- * Extended api with control over compression parameters
  compressWith,
  decompressWith,

  CompressParams(..), defaultCompressParams,
  DecompressParams(..), defaultDecompressParams,

  -- ** The compression parameter types
  CompressionLevel(..),
    defaultCompression,
    noCompression,
    bestSpeed,
    bestCompression,
    compressionLevel,
  Method(..),
    deflateMethod,
  WindowBits(..),
    defaultWindowBits,
    windowBits,
  MemoryLevel(..),
    defaultMemoryLevel,
    minMemoryLevel,
    maxMemoryLevel,
    memoryLevel,
  CompressionStrategy(..),
    defaultStrategy,
    filteredStrategy,
    huffmanOnlyStrategy,

  ) where

import Data.ByteString.Lazy (ByteString)

import qualified Codec.Compression.Zlib.Internal as Internal
import Codec.Compression.Zlib.Internal hiding (compress, decompress)

decompress :: ByteString -> ByteString
decompress = decompressWith defaultDecompressParams

decompressWith :: DecompressParams -> ByteString -> ByteString
decompressWith = Internal.decompress rawFormat

compress :: ByteString -> ByteString
compress = compressWith defaultCompressParams

compressWith :: CompressParams -> ByteString -> ByteString
compressWith = Internal.compress rawFormat
