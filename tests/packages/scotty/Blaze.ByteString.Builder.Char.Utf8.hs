{-# LINE 1 "Blaze/ByteString/Builder/Char/Utf8.hs" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- ignore warning from 'import Data.Text.Encoding'

-- |
-- Module      : Blaze.ByteString.Builder.Char.Utf8
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing Unicode characters using the UTF-8
-- encoding.
--
module Blaze.ByteString.Builder.Char.Utf8
    (
      -- * Writing UTF-8 encoded characters to a buffer
      writeChar

      -- * Creating Builders from UTF-8 encoded characters
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

-- | Write a UTF-8 encoded Unicode character to a buffer.
--
{-# INLINE writeChar #-}
writeChar :: Char -> Write
writeChar c = boundedWrite 4 (encodeCharUtf8 f1 f2 f3 f4 c)
  where
    f1 x1          = pokeN 1 $ \op -> do pokeByteOff op 0 x1

    f2 x1 x2       = pokeN 2 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2

    f3 x1 x2 x3    = pokeN 3 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3

    f4 x1 x2 x3 x4 = pokeN 4 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3
                                         pokeByteOff op 3 x4

-- | Encode a Unicode character to another datatype, using UTF-8. This function
-- acts as an abstract way of encoding characters, as it is unaware of what
-- needs to happen with the resulting bytes: you have to specify functions to
-- deal with those.
--
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8
               -> Char                                     -- ^ Input 'Char'
               -> a                                        -- ^ Result
encodeCharUtf8 f1 f2 f3 f4 c = case ord c of
    x | x <= 0x7F -> f1 $ fromIntegral x
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in f2 x1 x2
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f3 x1 x2 x3
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f4 x1 x2 x3 x4
{-# INLINE encodeCharUtf8 #-}

-- | /O(1)/. Serialize a Unicode character using the UTF-8 encoding.
--
fromChar :: Char -> Builder
fromChar = fromWriteSingleton writeChar

-- | /O(n)/. Serialize a Unicode 'String' using the UTF-8 encoding.
--
fromString :: String -> Builder
fromString = fromWriteList writeChar
-- Performance note: ^^^
--
--   fromWrite2List made things slightly worse for the blaze-html benchmarks
--   despite being better when serializing only a list.  Probably, the cache is
--   already occupied enough with dealing with the data from Html rendering.
--


-- | /O(n)/. Serialize a value by 'Show'ing it and UTF-8 encoding the resulting
-- 'String'.
--
fromShow :: Show a => a -> Builder
fromShow = fromString . show

-- | /O(n)/. Serialize a strict Unicode 'TS.Text' value using the UTF-8 encoding.
--
fromText :: TS.Text -> Builder
fromText = fromString . TS.unpack
{-# INLINE fromText #-}


-- | /O(n)/. Serialize a lazy Unicode 'TL.Text' value using the UTF-8 encoding.
--
fromLazyText :: TL.Text -> Builder
fromLazyText = fromString . TL.unpack
{-# INLINE fromLazyText #-}
