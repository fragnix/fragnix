{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Blaze/ByteString/Builder/Char8.hs" #-}
------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Char8
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- //Note:// This package is intended for low-level use like implementing
-- protocols. If you need to //serialize// Unicode characters use one of the
-- UTF encodings (e.g. 'Blaze.ByteString.Builder.Char.UTF-8').
--
-- 'Write's and 'Builder's for serializing the lower 8-bits of characters.
--
-- This corresponds to what the 'bytestring' package offer in
-- 'Data.ByteString.Char8'.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Char8
    (
      -- * Writing Latin-1 (ISO 8859-1) encodable characters to a buffer
      writeChar

      -- * Creating Builders from Latin-1 (ISO 8859-1) encodable characters
    , fromChar
    , fromString
    , fromShow
    , fromText
    , fromLazyText
    ) where

import Blaze.ByteString.Builder.Compat.Write ( Write, writePrimFixed )
import           Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.Text      as TS
import qualified Data.Text.Lazy as TL

-- | Write the lower 8-bits of a character to a buffer.
writeChar :: Char -> Write
writeChar = writePrimFixed P.char8
{-# INLINE writeChar #-}

-- | /O(1)/. Serialize the lower 8-bits of a character.
fromChar :: Char -> Builder
fromChar = B.char8
{-# INLINE fromChar #-}

-- | /O(n)/. Serialize the lower 8-bits of all characters of a string
fromString :: String -> Builder
fromString = P.primMapListFixed P.char8
{-# INLINE fromString #-}

-- | /O(n)/. Serialize a value by 'Show'ing it and serializing the lower 8-bits
-- of the resulting string.
fromShow :: Show a => a -> Builder
fromShow = fromString . show
{-# INLINE fromShow #-}

-- | /O(n)/. Serialize the lower 8-bits of all characters in the strict text.
fromText :: TS.Text -> Builder
fromText = fromString . TS.unpack
{-# INLINE fromText #-}

-- | /O(n)/. Serialize the lower 8-bits of all characters in the lazy text.
fromLazyText :: TL.Text -> Builder
fromLazyText = fromString . TL.unpack
{-# INLINE fromLazyText #-}
