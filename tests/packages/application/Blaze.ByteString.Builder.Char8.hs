{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Blaze/ByteString/Builder/Char8.hs" #-}













































-- ignore warning from 'import Data.Text.Encoding'
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE CPP #-}


-- |
-- Module      : Blaze.ByteString.Builder.Char8
-- Copyright   : (c) 2010 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
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

import Foreign
import Data.Char (ord)

import qualified Data.Text               as TS
import qualified Data.Text.Encoding      as TS -- imported for documentation links
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TS -- imported for documentation links

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Word

-- | Write the lower 8-bits of a character to a buffer.
--
{-# INLINE writeChar #-}
writeChar :: Char -> Write
writeChar = writeWord8 . fromIntegral . ord

-- | /O(1)/. Serialize the lower 8-bits of a character.
--
fromChar :: Char -> Builder
fromChar = fromWriteSingleton writeChar

-- | /O(n)/. Serialize the lower 8-bits of all characters of a string
--
fromString :: String -> Builder
fromString = fromWriteList writeChar

-- | /O(n)/. Serialize a value by 'Show'ing it and serializing the lower 8-bits
-- of the resulting string.
--
fromShow :: Show a => a -> Builder
fromShow = fromString . show

-- | /O(n)/. Serialize the lower 8-bits of all characters in the strict text.
--
{-# INLINE fromText #-}
fromText :: TS.Text -> Builder
fromText = fromString . TS.unpack

-- | /O(n)/. Serialize the lower 8-bits of all characters in the lazy text.
--
{-# INLINE fromLazyText #-}
fromLazyText :: TL.Text -> Builder
fromLazyText = fromString . TL.unpack
