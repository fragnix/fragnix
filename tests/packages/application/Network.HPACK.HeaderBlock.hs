{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/HeaderBlock.hs" #-}
module Network.HPACK.HeaderBlock (
    decodeHeader
  , decodeTokenHeader
  , ValueTable
  , toHeaderTable
  , getHeaderValue
  , encodeHeader
  , encodeTokenHeader
  ) where

import Network.HPACK.HeaderBlock.Decode
import Network.HPACK.HeaderBlock.Encode
