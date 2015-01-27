{-# LINE 1 "Blaze/ByteString/Builder/Html/Utf8.hs" #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Blaze.ByteString.Builder.Html.Utf8
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing HTML escaped and UTF-8 encoded
-- characters.
--
-- This module is used by both the 'blaze-html' and the \'hamlet\' HTML
-- templating libraries. If the 'Builder' from 'blaze-builder' replaces the
-- 'Data.Binary.Builder' implementation, this module will most likely keep its
-- place, as it provides a set of very specialized functions.
module Blaze.ByteString.Builder.Html.Utf8
    (
      module Blaze.ByteString.Builder.Char.Utf8

      -- * Writing HTML escaped and UTF-8 encoded characters to a buffer
    , writeHtmlEscapedChar

      -- * Creating Builders from HTML escaped and UTF-8 encoded characters
    , fromHtmlEscapedChar
    , fromHtmlEscapedString
    , fromHtmlEscapedShow
    , fromHtmlEscapedText
    , fromHtmlEscapedLazyText
    ) where

import Data.ByteString.Char8 ()  -- for the 'IsString' instance of bytesrings

import qualified Data.Text      as TS
import qualified Data.Text.Lazy as TL

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Char.Utf8

-- | Write a HTML escaped and UTF-8 encoded Unicode character to a bufffer.
--
writeHtmlEscapedChar :: Char -> Write
writeHtmlEscapedChar c0 =
    boundedWrite 6 (io c0)
    -- WARNING: Don't forget to change the bound if you change the bytestrings.
  where
    io '<'  = getPoke $ writeByteString "&lt;"
    io '>'  = getPoke $ writeByteString "&gt;"
    io '&'  = getPoke $ writeByteString "&amp;"
    io '"'  = getPoke $ writeByteString "&quot;"
    io '\'' = getPoke $ writeByteString "&#39;"
    io c    = getPoke $ writeChar c
{-# INLINE writeHtmlEscapedChar #-}

-- | /O(1)./ Serialize a HTML escaped Unicode character using the UTF-8
-- encoding.
--
fromHtmlEscapedChar :: Char -> Builder
fromHtmlEscapedChar = fromWriteSingleton writeHtmlEscapedChar

-- | /O(n)/. Serialize a HTML escaped Unicode 'String' using the UTF-8
-- encoding.
--
fromHtmlEscapedString :: String -> Builder
fromHtmlEscapedString = fromWriteList writeHtmlEscapedChar

-- | /O(n)/. Serialize a value by 'Show'ing it and then, HTML escaping and
-- UTF-8 encoding the resulting 'String'.
--
fromHtmlEscapedShow :: Show a => a -> Builder
fromHtmlEscapedShow = fromHtmlEscapedString . show


-- | /O(n)/. Serialize a HTML escaped strict Unicode 'TS.Text' value using the
-- UTF-8 encoding.
--
fromHtmlEscapedText :: TS.Text -> Builder
fromHtmlEscapedText = fromHtmlEscapedString . TS.unpack

-- | /O(n)/. Serialize a HTML escaped Unicode 'TL.Text' using the UTF-8 encoding.
--
fromHtmlEscapedLazyText :: TL.Text -> Builder
fromHtmlEscapedLazyText = fromHtmlEscapedString . TL.unpack

