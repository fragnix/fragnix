{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/Huffman.hs" #-}
module Network.HPACK.Huffman (
  -- * Type
    HuffmanEncoding
  , HuffmanDecoding
  -- * Encoding/decoding
  , encode
  , encodeHuffman
  , decode
  , decodeHuffman
  ) where

import Network.HPACK.Huffman.Decode
import Network.HPACK.Huffman.Encode
