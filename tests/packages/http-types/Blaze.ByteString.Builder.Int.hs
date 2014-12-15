{-# LINE 1 "./Blaze/ByteString/Builder/Int.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Blaze/ByteString/Builder/Int.hs" #-}
{-# LINE 1 "./Blaze/ByteString/Builder/Int.hs" #-}
{-# LANGUAGE CPP #-}





-- |
-- Module      : Blaze.ByteString.Builder.Int
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing integers.
--
-- See "Blaze.ByteString.Builder.Word" for information about how to best write several
-- integers at once.
--
module Blaze.ByteString.Builder.Int
    (
    -- * Writing integers to a buffer

      writeInt8

    -- ** Big-endian writes
    , writeInt16be           -- :: Int16 -> Write
    , writeInt32be           -- :: Int32 -> Write
    , writeInt64be           -- :: Int64 -> Write

    -- ** Little-endian writes
    , writeInt16le           -- :: Int16 -> Write
    , writeInt32le           -- :: Int32 -> Write
    , writeInt64le           -- :: Int64 -> Write

    -- ** Host-endian writes
    , writeInthost           -- :: Int -> Write
    , writeInt16host         -- :: Int16 -> Write
    , writeInt32host         -- :: Int32 -> Write
    , writeInt64host         -- :: Int64 -> Write

    -- * Creating builders from integers

    -- | We provide serialization functions both for singleton integers as well as
    -- for lists of integers. Using these list serialization functions is /much/ faster
    -- than using @mconcat . map fromInt/<n/>@, as the list serialization
    -- functions use a tighter inner loop.

    , fromInt8
    , fromInt8s

    -- ** Big-endian serialization
    , fromInt16be            -- :: Int16   -> Builder
    , fromInt32be            -- :: Int32   -> Builder
    , fromInt64be            -- :: Int64   -> Builder
    , fromInt32sbe           -- :: [Int32] -> Builder
    , fromInt16sbe           -- :: [Int16] -> Builder
    , fromInt64sbe           -- :: [Int64] -> Builder

    -- ** Little-endian serialization
    , fromInt16le            -- :: Int16   -> Builder
    , fromInt32le            -- :: Int32   -> Builder
    , fromInt64le            -- :: Int64   -> Builder
    , fromInt16sle           -- :: [Int16] -> Builder
    , fromInt32sle           -- :: [Int32] -> Builder
    , fromInt64sle           -- :: [Int64] -> Builder

    -- ** Host-endian serialization
    , fromInthost            -- :: Int     -> Builder
    , fromInt16host          -- :: Int16   -> Builder
    , fromInt32host          -- :: Int32   -> Builder
    , fromInt64host          -- :: Int64   -> Builder
    , fromIntshost           -- :: [Int]   -> Builder
    , fromInt16shost         -- :: [Int16] -> Builder
    , fromInt32shost         -- :: [Int32] -> Builder
    , fromInt64shost         -- :: [Int64] -> Builder

    ) where

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Word

import Foreign

------------------------------------------------------------------------------
-- Int writes
--------------
--
-- we rely on 'fromIntegral' to do a loss-less conversion to the corresponding
-- 'Word' type
--
------------------------------------------------------------------------------


-- | Write a single signed byte.
--
writeInt8 :: Int8 -> Write
writeInt8 = writeWord8 . fromIntegral
{-# INLINE writeInt8 #-}

-- | Write an 'Int16' in big endian format.
writeInt16be :: Int16 -> Write
writeInt16be = writeWord16be . fromIntegral
{-# INLINE writeInt16be #-}

-- | Write an 'Int16' in little endian format.
writeInt16le :: Int16 -> Write
writeInt16le = writeWord16le . fromIntegral
{-# INLINE writeInt16le #-}

-- | Write an 'Int32' in big endian format.
writeInt32be :: Int32 -> Write
writeInt32be = writeWord32be . fromIntegral
{-# INLINE writeInt32be #-}

-- | Write an 'Int32' in little endian format.
writeInt32le :: Int32 -> Write
writeInt32le = writeWord32le . fromIntegral
{-# INLINE writeInt32le #-}

-- | Write an 'Int64' in big endian format.
writeInt64be :: Int64 -> Write
writeInt64be = writeWord64be . fromIntegral
{-# INLINE writeInt64be #-}

-- | Write an 'Int64' in little endian format.
writeInt64le :: Int64 -> Write
writeInt64le = writeWord64le . fromIntegral
{-# INLINE writeInt64le #-}


------------------------------------------------------------------------
-- Unaligned, integer size ops

-- | Write a single native machine 'Int'. The 'Int' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
writeInthost :: Int -> Write
writeInthost = writeStorable
{-# INLINE writeInthost #-}

-- | Write an 'Int16' in native host order and host endianness.
writeInt16host :: Int16 -> Write
writeInt16host = writeStorable
{-# INLINE writeInt16host #-}

-- | Write an 'Int32' in native host order and host endianness.
writeInt32host :: Int32 -> Write
writeInt32host = writeStorable
{-# INLINE writeInt32host #-}

-- | Write an 'Int64' in native host order and host endianness.
writeInt64host :: Int64 -> Write
writeInt64host = writeStorable
{-# INLINE writeInt64host #-}


------------------------------------------------------------------------------
-- Builders corresponding to the integer writes
------------------------------------------------------------------------------

-- Single bytes
------------------------------------------------------------------------------

-- | Serialize a single byte.
--
fromInt8 :: Int8 -> Builder
fromInt8 = fromWriteSingleton writeInt8

-- | Serialize a list of bytes.
--
fromInt8s :: [Int8] -> Builder
fromInt8s = fromWriteList writeInt8


-- Int16
------------------------------------------------------------------------------

-- | Serialize an 'Int16' in big endian format.
fromInt16be :: Int16 -> Builder
fromInt16be = fromWriteSingleton writeInt16be
{-# INLINE fromInt16be #-}

-- | Serialize a list of 'Int16's in big endian format.
fromInt16sbe :: [Int16] -> Builder
fromInt16sbe = fromWriteList writeInt16be
{-# INLINE fromInt16sbe #-}

-- | Serialize an 'Int16' in little endian format.
fromInt16le :: Int16 -> Builder
fromInt16le = fromWriteSingleton writeInt16le
{-# INLINE fromInt16le #-}

-- | Serialize a list of 'Int16's in little endian format.
fromInt16sle :: [Int16] -> Builder
fromInt16sle = fromWriteList writeInt16le
{-# INLINE fromInt16sle #-}


-- Int32
-----------------------------------------------------------------------------

-- | Serialize an 'Int32' in big endian format.
fromInt32be :: Int32 -> Builder
fromInt32be = fromWriteSingleton writeInt32be
{-# INLINE fromInt32be #-}

-- | Serialize a list of 'Int32's in big endian format.
fromInt32sbe :: [Int32] -> Builder
fromInt32sbe = fromWriteList writeInt32be
{-# INLINE fromInt32sbe #-}

-- | Serialize an 'Int32' in little endian format.
fromInt32le :: Int32 -> Builder
fromInt32le = fromWriteSingleton writeInt32le
{-# INLINE fromInt32le #-}

-- | Serialize a list of 'Int32's in little endian format.
fromInt32sle :: [Int32] -> Builder
fromInt32sle = fromWriteList writeInt32le
{-# INLINE fromInt32sle #-}

-- | Serialize an 'Int64' in big endian format.
fromInt64be :: Int64 -> Builder
fromInt64be = fromWriteSingleton writeInt64be
{-# INLINE fromInt64be #-}

-- | Serialize a list of 'Int64's in big endian format.
fromInt64sbe :: [Int64] -> Builder
fromInt64sbe = fromWriteList writeInt64be
{-# INLINE fromInt64sbe #-}

-- | Serialize an 'Int64' in little endian format.
fromInt64le :: Int64 -> Builder
fromInt64le = fromWriteSingleton writeInt64le
{-# INLINE fromInt64le #-}

-- | Serialize a list of 'Int64's in little endian format.
fromInt64sle :: [Int64] -> Builder
fromInt64sle = fromWriteList writeInt64le
{-# INLINE fromInt64sle #-}


------------------------------------------------------------------------
-- Unaligned, integer size ops

-- | Serialize a single native machine 'Int'. The 'Int' is serialized in host
-- order, host endian form, for the machine you're on. On a 64 bit machine the
-- 'Int' is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this
-- way are not portable to different endian or integer sized machines, without
-- conversion.
--
fromInthost :: Int -> Builder
fromInthost = fromWriteSingleton writeInthost
{-# INLINE fromInthost #-}

-- | Serialize a list of 'Int's.
-- See 'fromInthost' for usage considerations.
fromIntshost :: [Int] -> Builder
fromIntshost = fromWriteList writeInthost
{-# INLINE fromIntshost #-}

-- | Write an 'Int16' in native host order and host endianness.
fromInt16host :: Int16 -> Builder
fromInt16host = fromWriteSingleton writeInt16host
{-# INLINE fromInt16host #-}

-- | Write a list of 'Int16's in native host order and host endianness.
fromInt16shost :: [Int16] -> Builder
fromInt16shost = fromWriteList writeInt16host
{-# INLINE fromInt16shost #-}

-- | Write an 'Int32' in native host order and host endianness.
fromInt32host :: Int32 -> Builder
fromInt32host = fromWriteSingleton writeInt32host
{-# INLINE fromInt32host #-}

-- | Write a list of 'Int32's in native host order and host endianness.
fromInt32shost :: [Int32] -> Builder
fromInt32shost = fromWriteList writeInt32host
{-# INLINE fromInt32shost #-}

-- | Write an 'Int64' in native host order and host endianness.
fromInt64host :: Int64 -> Builder
fromInt64host = fromWriteSingleton writeInt64host
{-# INLINE fromInt64host #-}

-- | Write a list of 'Int64's in native host order and host endianness.
fromInt64shost :: [Int64] -> Builder
fromInt64shost = fromWriteList writeInt64host
{-# INLINE fromInt64shost #-}
