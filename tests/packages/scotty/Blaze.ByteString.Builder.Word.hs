{-# LINE 1 "Blaze/ByteString/Builder/Word.hs" #-}
# 1 "Blaze/ByteString/Builder/Word.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1





























































































# 9 "<command-line>" 2
# 1 "Blaze/ByteString/Builder/Word.hs"
{-# LANGUAGE CPP #-}





-- |
-- Module      : Blaze.ByteString.Builder.Word
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
--
--               Original serialization code from 'Data.Binary.Builder':
--               (c) Lennart Kolmodin, Ross Patterson
--
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
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


# 1 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h" 1

# 15 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h"






# 1 "/usr/local/lib/ghc-7.8.3/include/ghcautoconf.h" 1





































































































































































































































































































































































































































# 21 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h" 2

































































# 99 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h"

# 109 "/usr/local/lib/ghc-7.8.3/include/MachDeps.h"











# 38 "Blaze/ByteString/Builder/Word.hs" 2

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

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Internal.UncheckedShifts

import Foreign

------------------------------------------------------------------------------
-- Word writes
--------------
--
-- Based upon the 'putWordX' functions from "Data.Binary.Builder" from the
-- 'binary' package.
--
------------------------------------------------------------------------------


-- | Write a single byte.
--
writeWord8 :: Word8 -> Write
writeWord8 x = exactWrite 1 (\pf -> poke pf x)
{-# INLINE writeWord8 #-}

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a 'Word16' in big endian format.
writeWord16be :: Word16 -> Write
writeWord16be w = exactWrite 2 $ \p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
{-# INLINE writeWord16be #-}

-- | Write a 'Word16' in little endian format.
writeWord16le :: Word16 -> Write
writeWord16le w = exactWrite 2 $ \p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
{-# INLINE writeWord16le #-}

-- writeWord16le w16 = exactWrite 2 (\p -> poke (castPtr p) w16)

-- | Write a 'Word32' in big endian format.
writeWord32be :: Word32 -> Write
writeWord32be w = exactWrite 4 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
{-# INLINE writeWord32be #-}

-- | Write a 'Word32' in little endian format.
writeWord32le :: Word32 -> Write
writeWord32le w = exactWrite 4 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
{-# INLINE writeWord32le #-}

-- on a little endian machine:
-- writeWord32le w32 = exactWrite 4 (\p -> poke (castPtr p) w32)

-- | Write a 'Word64' in big endian format.
writeWord64be :: Word64 -> Write
# 182 "Blaze/ByteString/Builder/Word.hs"
writeWord64be w = exactWrite 8 $ \p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)

{-# INLINE writeWord64be #-}

-- | Write a 'Word64' in little endian format.
writeWord64le :: Word64 -> Write

# 211 "Blaze/ByteString/Builder/Word.hs"
writeWord64le w = exactWrite 8 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)

{-# INLINE writeWord64le #-}

-- on a little endian machine:
-- writeWord64le w64 = exactWrite 8 (\p -> poke (castPtr p) w64)

------------------------------------------------------------------------
-- Unaligned, word size ops

-- | Write a single native machine 'Word'. The 'Word' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
writeWordhost :: Word -> Write
writeWordhost w =
    exactWrite (sizeOf (undefined :: Word)) (\p -> poke (castPtr p) w)
{-# INLINE writeWordhost #-}

-- | Write a 'Word16' in native host order and host endianness.
writeWord16host :: Word16 -> Write
writeWord16host w16 =
    exactWrite (sizeOf (undefined :: Word16)) (\p -> poke (castPtr p) w16)
{-# INLINE writeWord16host #-}

-- | Write a 'Word32' in native host order and host endianness.
writeWord32host :: Word32 -> Write
writeWord32host w32 =
    exactWrite (sizeOf (undefined :: Word32)) (\p -> poke (castPtr p) w32)
{-# INLINE writeWord32host #-}

-- | Write a 'Word64' in native host order and host endianness.
writeWord64host :: Word64 -> Write
writeWord64host w =
    exactWrite (sizeOf (undefined :: Word64)) (\p -> poke (castPtr p) w)
{-# INLINE writeWord64host #-}


------------------------------------------------------------------------------
-- Builders corresponding to the word writes
------------------------------------------------------------------------------

-- Single bytes
------------------------------------------------------------------------------

-- | Serialize a single byte.
--
fromWord8 :: Word8 -> Builder
fromWord8 = fromWriteSingleton writeWord8

-- | Serialize a list of bytes.
--
fromWord8s :: [Word8] -> Builder
fromWord8s = fromWriteList writeWord8


-- Word16
------------------------------------------------------------------------------

-- | Serialize a 'Word16' in big endian format.
fromWord16be :: Word16 -> Builder
fromWord16be = fromWriteSingleton writeWord16be
{-# INLINE fromWord16be #-}

-- | Serialize a list of 'Word16's in big endian format.
fromWord16sbe :: [Word16] -> Builder
fromWord16sbe = fromWriteList writeWord16be
{-# INLINE fromWord16sbe #-}

-- | Serialize a 'Word16' in little endian format.
fromWord16le :: Word16 -> Builder
fromWord16le = fromWriteSingleton writeWord16le
{-# INLINE fromWord16le #-}

-- | Serialize a list of 'Word16's in little endian format.
fromWord16sle :: [Word16] -> Builder
fromWord16sle = fromWriteList writeWord16le
{-# INLINE fromWord16sle #-}


-- Word32
-----------------------------------------------------------------------------

-- | Serialize a 'Word32' in big endian format.
fromWord32be :: Word32 -> Builder
fromWord32be = fromWriteSingleton writeWord32be
{-# INLINE fromWord32be #-}

-- | Serialize a list of 'Word32's in big endian format.
fromWord32sbe :: [Word32] -> Builder
fromWord32sbe = fromWriteList writeWord32be
{-# INLINE fromWord32sbe #-}

-- | Serialize a 'Word32' in little endian format.
fromWord32le :: Word32 -> Builder
fromWord32le = fromWriteSingleton writeWord32le
{-# INLINE fromWord32le #-}

-- | Serialize a list of 'Word32's in little endian format.
fromWord32sle :: [Word32] -> Builder
fromWord32sle = fromWriteList writeWord32le
{-# INLINE fromWord32sle #-}

-- | Serialize a 'Word64' in big endian format.
fromWord64be :: Word64 -> Builder
fromWord64be = fromWriteSingleton writeWord64be
{-# INLINE fromWord64be #-}

-- | Serialize a list of 'Word64's in big endian format.
fromWord64sbe :: [Word64] -> Builder
fromWord64sbe = fromWriteList writeWord64be
{-# INLINE fromWord64sbe #-}

-- | Serialize a 'Word64' in little endian format.
fromWord64le :: Word64 -> Builder
fromWord64le = fromWriteSingleton writeWord64le
{-# INLINE fromWord64le #-}

-- | Serialize a list of 'Word64's in little endian format.
fromWord64sle :: [Word64] -> Builder
fromWord64sle = fromWriteList writeWord64le
{-# INLINE fromWord64sle #-}


------------------------------------------------------------------------
-- Unaligned, word size ops

-- | Serialize a single native machine 'Word'. The 'Word' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Word' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or word sized machines, without
-- conversion.
--
fromWordhost :: Word -> Builder
fromWordhost = fromWriteSingleton writeWordhost
{-# INLINE fromWordhost #-}

-- | Serialize a list of 'Word's.
-- See 'fromWordhost' for usage considerations.
fromWordshost :: [Word] -> Builder
fromWordshost = fromWriteList writeWordhost
{-# INLINE fromWordshost #-}

-- | Write a 'Word16' in native host order and host endianness.
fromWord16host :: Word16 -> Builder
fromWord16host = fromWriteSingleton writeWord16host
{-# INLINE fromWord16host #-}

-- | Write a list of 'Word16's in native host order and host endianness.
fromWord16shost :: [Word16] -> Builder
fromWord16shost = fromWriteList writeWord16host
{-# INLINE fromWord16shost #-}

-- | Write a 'Word32' in native host order and host endianness.
fromWord32host :: Word32 -> Builder
fromWord32host = fromWriteSingleton writeWord32host
{-# INLINE fromWord32host #-}

-- | Write a list of 'Word32's in native host order and host endianness.
fromWord32shost :: [Word32] -> Builder
fromWord32shost = fromWriteList writeWord32host
{-# INLINE fromWord32shost #-}

-- | Write a 'Word64' in native host order and host endianness.
fromWord64host :: Word64 -> Builder
fromWord64host = fromWriteSingleton writeWord64host
{-# INLINE fromWord64host #-}

-- | Write a list of 'Word64's in native host order and host endianness.
fromWord64shost :: [Word64] -> Builder
fromWord64shost = fromWriteList writeWord64host
{-# INLINE fromWord64shost #-}
