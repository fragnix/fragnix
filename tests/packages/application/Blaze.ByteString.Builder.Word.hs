{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Blaze/ByteString/Builder/Word.hs" #-}
------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Word
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- 'Write's and 'Builder's for serializing words.
--
-- Note that for serializing a three tuple @(x,y,z)@ of bytes (or other word
-- values) you should use the expression
--
-- > fromWrite $ writeWord8 x `mappend` writeWord8 y `mappend` writeWord z
--
-- instead of
--
-- > fromWord8 x `mappend` fromWord8 y `mappend` fromWord z
--
-- The first expression will result in a single atomic write of three bytes,
-- while the second expression will check for each byte, if there is free space
-- left in the output buffer. Coalescing these checks can improve performance
-- quite a bit, as long as you use it sensibly.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Word
    (
    -- * Writing words to a buffer

      writeWord8

    -- ** Big-endian writes
    , writeWord16be           -- :: Word16 -> Write
    , writeWord32be           -- :: Word32 -> Write
    , writeWord64be           -- :: Word64 -> Write

    -- ** Little-endian writes
    , writeWord16le           -- :: Word16 -> Write
    , writeWord32le           -- :: Word32 -> Write
    , writeWord64le           -- :: Word64 -> Write

    -- ** Host-endian writes
    , writeWordhost           -- :: Word -> Write
    , writeWord16host         -- :: Word16 -> Write
    , writeWord32host         -- :: Word32 -> Write
    , writeWord64host         -- :: Word64 -> Write

    -- * Creating builders from words

    -- | We provide serialization functions both for singleton words as well as
    -- for lists of words. Using these list serialization functions is /much/ faster
    -- than using @mconcat . map fromWord/<n/>@, as the list serialization
    -- functions use a tighter inner loop.

    , fromWord8
    , fromWord8s

    -- ** Big-endian serialization
    , fromWord16be            -- :: Word16   -> Builder
    , fromWord32be            -- :: Word32   -> Builder
    , fromWord64be            -- :: Word64   -> Builder
    , fromWord32sbe           -- :: [Word32] -> Builder
    , fromWord16sbe           -- :: [Word16] -> Builder
    , fromWord64sbe           -- :: [Word64] -> Builder

    -- ** Little-endian serialization
    , fromWord16le            -- :: Word16   -> Builder
    , fromWord32le            -- :: Word32   -> Builder
    , fromWord64le            -- :: Word64   -> Builder
    , fromWord16sle           -- :: [Word16] -> Builder
    , fromWord32sle           -- :: [Word32] -> Builder
    , fromWord64sle           -- :: [Word64] -> Builder

    -- ** Host-endian serialization
    , fromWordhost            -- :: Word     -> Builder
    , fromWord16host          -- :: Word16   -> Builder
    , fromWord32host          -- :: Word32   -> Builder
    , fromWord64host          -- :: Word64   -> Builder
    , fromWordshost           -- :: [Word]   -> Builder
    , fromWord16shost         -- :: [Word16] -> Builder
    , fromWord32shost         -- :: [Word32] -> Builder
    , fromWord64shost         -- :: [Word64] -> Builder

    ) where

import Data.Word
import Blaze.ByteString.Builder.Compat.Write ( Write, writePrimFixed )
import           Data.ByteString.Builder ( Builder )
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Builder.Prim  as P

-- | Write a single byte.
--
writeWord8 :: Word8 -> Write
writeWord8 = writePrimFixed P.word8
{-# INLINE writeWord8 #-}

-- | Write a 'Word16' in big endian format.
writeWord16be :: Word16 -> Write
writeWord16be = writePrimFixed P.word16BE
{-# INLINE writeWord16be #-}

-- | Write a 'Word32' in big endian format.
writeWord32be :: Word32 -> Write
writeWord32be = writePrimFixed P.word32BE
{-# INLINE writeWord32be #-}

-- | Write a 'Word64' in big endian format.
writeWord64be :: Word64 -> Write
writeWord64be = writePrimFixed P.word64BE
{-# INLINE writeWord64be #-}

-- | Write a 'Word16' in little endian format.
writeWord16le :: Word16 -> Write
writeWord16le = writePrimFixed P.word16LE
{-# INLINE writeWord16le #-}

-- | Write a 'Word32' in big endian format.
writeWord32le :: Word32 -> Write
writeWord32le = writePrimFixed P.word32LE
{-# INLINE writeWord32le #-}

-- | Write a 'Word64' in little endian format.
writeWord64le :: Word64 -> Write
writeWord64le = writePrimFixed P.word64LE
{-# INLINE writeWord64le #-}

-- | Write a single native machine 'Word'. The 'Word' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
writeWordhost :: Word -> Write
writeWordhost = writePrimFixed P.wordHost
{-# INLINE writeWordhost #-}

-- | Write a 'Word16' in native host order and host endianness.
writeWord16host :: Word16 -> Write
writeWord16host = writePrimFixed P.word16Host
{-# INLINE writeWord16host #-}

-- | Write a 'Word32' in native host order and host endianness.
writeWord32host :: Word32 -> Write
writeWord32host = writePrimFixed P.word32Host
{-# INLINE writeWord32host #-}

-- | Write a 'Word64' in native host order and host endianness.
writeWord64host :: Word64 -> Write
writeWord64host = writePrimFixed P.word64Host
{-# INLINE writeWord64host #-}

-- | Serialize a single byte.
fromWord8 :: Word8 -> Builder
fromWord8 = B.word8
{-# INLINE fromWord8 #-}

-- | Serialize a list of bytes.
fromWord8s :: [Word8] -> Builder
fromWord8s = P.primMapListFixed P.word8
{-# INLINE fromWord8s #-}

-- | Serialize a 'Word16' in big endian format.
fromWord16be :: Word16   -> Builder
fromWord16be = B.word16BE
{-# INLINE fromWord16be #-}

-- | Serialize a 'Word32' in big endian format.
fromWord32be :: Word32   -> Builder
fromWord32be = B.word32BE
{-# INLINE fromWord32be #-}

-- | Serialize a 'Word64' in big endian format.
fromWord64be :: Word64   -> Builder
fromWord64be = B.word64BE
{-# INLINE fromWord64be #-}

-- | Serialize a list of 'Word32's in big endian format.
fromWord32sbe :: [Word32] -> Builder
fromWord32sbe = P.primMapListFixed P.word32BE
{-# INLINE fromWord32sbe #-}

-- | Serialize a list of 'Word16's in big endian format.
fromWord16sbe :: [Word16] -> Builder
fromWord16sbe = P.primMapListFixed P.word16BE
{-# INLINE fromWord16sbe #-}

-- | Serialize a list of 'Word64's in big endian format.
fromWord64sbe :: [Word64] -> Builder
fromWord64sbe = P.primMapListFixed P.word64BE
{-# INLINE fromWord64sbe #-}

-- | Serialize a 'Word16' in little endian format.
fromWord16le :: Word16   -> Builder
fromWord16le = B.word16LE
{-# INLINE fromWord16le #-}

-- | Serialize a list of 'Word32's in little endian format.
fromWord32le :: Word32   -> Builder
fromWord32le = B.word32LE
{-# INLINE fromWord32le #-}

-- | Serialize a 'Word64' in little endian format.
fromWord64le :: Word64   -> Builder
fromWord64le = B.word64LE
{-# INLINE fromWord64le #-}

-- | Serialize a list of 'Word16's in little endian format.
fromWord16sle :: [Word16] -> Builder
fromWord16sle = P.primMapListFixed P.word16LE
{-# INLINE fromWord16sle #-}

-- | Serialize a list of 'Word32's in little endian format.
fromWord32sle :: [Word32] -> Builder
fromWord32sle = P.primMapListFixed P.word32LE
{-# INLINE fromWord32sle #-}

-- | Serialize a list of 'Word64's in little endian format.
fromWord64sle :: [Word64] -> Builder
fromWord64sle = P.primMapListFixed P.word64LE
{-# INLINE fromWord64sle #-}

-- | Serialize a single native machine 'Word'. The 'Word' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Word' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or word sized machines, without
-- conversion.
fromWordhost :: Word     -> Builder
fromWordhost = B.wordHost
{-# INLINE fromWordhost #-}

-- | Write a 'Word16' in native host order and host endianness.
fromWord16host :: Word16   -> Builder
fromWord16host = B.word16Host
{-# INLINE fromWord16host #-}

-- | Write a 'Word32' in native host order and host endianness.
fromWord32host :: Word32   -> Builder
fromWord32host = B.word32Host
{-# INLINE fromWord32host #-}

-- | Write a 'Word64' in native host order and host endianness.
fromWord64host :: Word64   -> Builder
fromWord64host = B.word64Host
{-# INLINE fromWord64host #-}

-- | Serialize a list of 'Word's.
-- See 'fromWordhost' for usage considerations.
fromWordshost :: [Word]   -> Builder
fromWordshost = P.primMapListFixed P.wordHost
{-# INLINE fromWordshost #-}

-- | Write a list of 'Word16's in native host order and host endianness.
fromWord16shost :: [Word16] -> Builder
fromWord16shost = P.primMapListFixed P.word16Host
{-# INLINE fromWord16shost #-}

-- | Write a list of 'Word32's in native host order and host endianness.
fromWord32shost :: [Word32] -> Builder
fromWord32shost = P.primMapListFixed P.word32Host
{-# INLINE fromWord32shost #-}

-- | Write a 'Word64' in native host order and host endianness.
fromWord64shost :: [Word64] -> Builder
fromWord64shost = P.primMapListFixed P.word64Host
{-# INLINE fromWord64shost #-}
