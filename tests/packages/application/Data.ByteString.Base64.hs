{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/ByteString/Base64.hs" #-}

















































{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Data.ByteString.Base64
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base64-encoded strings.

module Data.ByteString.Base64
    (
      encode
    , decode
    , decodeLenient
    , joinWith
    ) where

import Data.ByteString.Base64.Internal
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

-- | Encode a string into base64 form.  The result will always be a
-- multiple of 4 bytes in length.
encode :: ByteString -> ByteString
encode s = encodeWith (mkEncodeTable alphabet) s

-- | Decode a base64-encoded string.  This function strictly follows
-- the specification in RFC 4648,
-- <http://www.apps.ietf.org/rfc/rfc4648.html>.
decode :: ByteString -> Either String ByteString
decode s = decodeWithTable decodeFP s

-- | Decode a base64-encoded string.  This function is lenient in
-- following the specification from RFC 4648,
-- <http://www.apps.ietf.org/rfc/rfc4648.html>, and will not generate
-- parse errors no matter how poor its input.
decodeLenient :: ByteString -> ByteString
decodeLenient s = decodeLenientWithTable decodeFP s

alphabet :: ByteString
alphabet = B.pack $ [65..90] ++ [97..122] ++ [48..57] ++ [43,47]
{-# NOINLINE alphabet #-}

decodeFP :: ForeignPtr Word8
PS decodeFP _ _ = B.pack $
  replicate 43 x ++ [62,x,x,x,63] ++ [52..61] ++ [x,x,x,done,x,x,x] ++
  [0..25] ++ [x,x,x,x,x,x] ++ [26..51] ++ replicate 133 x
{-# NOINLINE decodeFP #-}

x :: Integral a => a
x = 255
{-# INLINE x #-}
